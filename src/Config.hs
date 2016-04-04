{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Config where


import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Conduit
import Data.IORef
import Control.Concurrent
import Control.Monad
import System.Environment
import Data.Maybe
import qualified Control.Exception    as E
import qualified Data.ByteString.Char8 as W


-- see: https://gist.github.com/michaelneale/1518cf42eb66d4818f33 && http://flapjack.io/docs/1.0/development/DATA_STRUCTURES/
data Check = 
  Check {
    check :: Text,
    api_key :: Text,
    tags :: [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON Check
instance FromJSON Check

toUrl :: Check -> String
toUrl chk = unpack (check chk)

  

-- Dyanmically refresh the config - magic.
dynamicConf :: IO (IORef [Check])
dynamicConf = do
  newRef <- newIORef []
  _ <- forkIO $ syncConfig newRef
  return newRef


fetchC :: String -> String -> IO (Maybe [Check])
fetchC url auth = withManager $ \manager -> do
                    res <- httpLbs r manager -- httpLbs for simpler fetching of body content                                                                                res <- httpLbs reqHead manager -- httpLbs for simpler fetching of body content                                        
                    return $ decode (responseBody res)
                    where
                      r = applyBasicAuth (W.pack auth) "" (confUrl url) {
                            method = "GET",
                            checkStatus = \_ _  _ -> Nothing  -- stops it barfing on non 200OK FFS.                      
                            }

ld :: IO (Maybe [Check])
ld = do
  url <- env "CONSOLE_URL" "https://radalert.io/api/v1/checks/http.pingbot"
  auth <-  env "CONSOLE_AUTH" "r4d4l3rt"
  result <- E.try (fetchC url auth) :: IO (Either HttpException (Maybe [Check]))
  case result of 
    Left e -> 
      do 
        print e
        return Nothing
    Right v -> return v


syncConfig :: IORef [Check] -> IO ()
syncConfig checksRef = do    
    putStrLn $ "Monitoring rad app for check changes now"
    forever $ do
       putStrLn $ "-> starting check for changes"       
       loadedChecks <- ld
       case loadedChecks of
          Nothing -> putStrLn "WARNING: Unable to decode checks from rad alert app"
          Just newChecks -> maybeUpdateChecks checksRef newChecks
       threadDelay 60000000
       return ()


maybeUpdateChecks :: IORef [Check] -> [Check] -> IO ()
maybeUpdateChecks existing new = do
  ex <- readIORef existing
  if ex == new 
    then 
      putStrLn "-> No changes needed at this time... "        
    else 
      putStrLn "NOTE: changes are being applied" >>
      modifyIORef existing (\_ -> new)
      

confUrl :: String -> Request
confUrl url = 
  case parseUrl url of
        Nothing -> error "Unable to load the url for looking up checks"
        Just req -> req

  
env :: String -> String -> IO String
env keyName defaultVal = do
  val <- lookupEnv keyName
  return $ fromMaybe defaultVal val
  
  

  
