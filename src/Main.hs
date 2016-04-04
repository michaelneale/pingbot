{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


import Control.Concurrent.Async (mapConcurrently)
import Control.Monad
import Control.Concurrent
import Network.HTTP.Conduit (Request, parseUrl)
import System.Environment
import Data.IORef
import System.Random


import Fetcher
import Notifier
import qualified Config as C


main :: IO ()
main = do
    confList <- C.dynamicConf 
    heartUrl <- getEnv "HEART_URL"
    commenceChecking confList heartUrl
    putStrLn "World is now ending. Have a nice day."


forkChecks :: String -> [C.Check] -> IO ()
forkChecks heartUrl confList = do
  _ <- mapConcurrently (doCheck heartUrl) confList
  return ()

-- run forever and forkIO each time we need to check, with the fresh list
commenceChecking :: IORef [C.Check] -> String -> IO ()
commenceChecking checkRef heartUrl = 
    forever $ do
       confList <- readIORef checkRef
       _ <- forkIO $ forkChecks heartUrl confList
       threadDelay 30000000
       
-- a one off check       
doCheck :: String -> C.Check -> IO ()
doCheck heartbeatUrl url = do    
    dither <- randomRIO (1, 10000000)                            
    threadDelay dither
    putStrLn $ "checking " ++ (C.toUrl url) ++ " with dither: " ++ (show dither)
    checkRes <- checkUrl url
    notifyFor hTarget url checkRes      
   where
     hTarget = heartTarget heartbeatUrl     
     

        

heartTarget :: String -> Request
heartTarget url =  
  case parseUrl url of --"http://requestb.in/scj9rysc" of 
    Nothing -> error "Not a valid heartbeat url"
    Just req -> req
      

  
--urlList :: [String]      
--urlList =  map (\v -> "http://www.google.com/" ++ show v) [1..5]
