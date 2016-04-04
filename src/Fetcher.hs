{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Fetcher 
          where

import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.Connection
import qualified Data.ByteString.Lazy as L
import qualified Control.Exception    as E
import qualified Config as C
import qualified Metrics as M
import Data.Time
import Network.URI 
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)



import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI


data StatusCheck = OK | BadUrl | BadStatus {responseCode :: Int} | BadResponse { errorMsg :: String} | MetricsChecks M.MetricsResponse

instance Show StatusCheck where
  show OK = "OK"
  show BadUrl = "Poorly formed URL"
  show (BadStatus c) = "Unexpected response code " ++ (show c)
  show (BadResponse e) = "Bad response " ++ e  
  show (MetricsChecks m) = "Metrics result " ++ (show m)



-- | Get a new Manager that doesn't verify SSL certificates
noSSLVerifyManager :: IO Manager
noSSLVerifyManager = let tlsSettings = TLSSettingsSimple {
                            -- This is where we disable certificate verification
                            settingDisableCertificateValidation = True,
                            settingDisableSession=False,
                            settingUseServerName=True}
                     in newManager $ mkManagerSettings tlsSettings Nothing
  


performCheck :: Manager -> C.Check -> IO StatusCheck
performCheck manager url = 
  --case parseUrl (C.toUrl url) of
  case parseTheUrl (C.toUrl url) of
        Nothing -> return BadUrl
        Just req -> do
            --_ <- liftIO $ print req
            fmap verifyResponse $  httpLbs (makeReq req "GET") manager -- httpLbs for simpler fetching of body content                                                                        


parseTheUrl :: String -> Maybe Request
parseTheUrl hurl = do
    uri <- parseURI hurl 
    parseAuth hurl $ uriAuthority uri

parseAuth :: String -> Maybe URIAuth -> Maybe Request
parseAuth hurl Nothing = parseUrl hurl
parseAuth hurl (Just URIAuth{uriUserInfo=""}) = parseUrl hurl
parseAuth hurl (Just uAuth) = do
    req <- parseUrl scrubUrl
    return $ applyAuth (S8.pack justCreds) req
  where    
    uInfo = uriUserInfo uAuth 
    justCreds = take (length uInfo - 1) uInfo --strip the @
    scrubUrl = T.unpack $ T.replace (T.pack uInfo) "" (T.pack hurl)
    

  
applyAuth :: S.ByteString -> Request -> Request
applyAuth auth req =
    req { requestHeaders = authHeader : requestHeaders req }
  where
    authHeader = (CI.mk "Authorization", basic)
    basic = S8.append "Basic " (B64.encode auth)
    
verifyResponse :: Response L.ByteString -> StatusCheck
verifyResponse r = 
  if respCode >= 500
  then
    BadStatus {responseCode = respCode} 
  else
    case M.parseMetrics (responseBody r) of 
      Nothing -> OK
      Just mr -> MetricsChecks mr
  where 
    respCode = statusCode $ responseStatus r
    


checkUrl :: C.Check -> IO (StatusCheck, Maybe Time)
checkUrl url =
  do
    start <- getCurrentTime
    manager <- noSSLVerifyManager
    result <- E.try $ performCheck  manager url :: IO (Either HttpException StatusCheck)
    closeManager manager
    end <- getCurrentTime    
    let diff = (realToFrac $ diffUTCTime end start) * 1000
    _ <- putStrLn $ "Check " ++ (show $ C.check url) ++ " response time " ++ (show diff)
    case result of 
      Left e -> 
        do 
          print e
          return (BadResponse { errorMsg = (show e) }, Nothing) 
      Right v -> return (v, Just diff)

type Time = Double

showDiff :: Double -> IO ()
showDiff d = do
  putStrLn "elapsed:"
  print d

httpTimeout :: Int
httpTimeout = 10000000

makeReq :: Request -> Method -> Request
makeReq req httpMethod = 
  req {
    method = httpMethod,
    checkStatus = \_ _  _ -> Nothing,  -- stops it barfing on non 200OK FFS.                      
    responseTimeout = Just httpTimeout,
    requestHeaders = ("User-Agent", "RadAlertBot/1.0 (+http://radalert.io)") : requestHeaders req
  }
  
  
