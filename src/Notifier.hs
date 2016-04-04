{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Notifier (
    notifyFor
) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Data.Text
import GHC.Generics

import qualified Config as C
import qualified Fetcher as F
import qualified Control.Exception    as E
import qualified Metrics as M
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM

data Event =
  Event {	 
	   state :: Maybe Text,
     check :: Text, 
     summary :: Maybe Text,
     api_key :: Text,
     metric :: Maybe Double,
     tags :: [Text]
  } deriving (Show, Generic)

--instance ToJSON Event


instance ToJSON Event where
    toJSON (ev) =
      objectNoNulls [ 
        "state"     .= (state ev)
      , "check"     .= (check ev)
      , "summary"   .= (summary ev)
      , "api_key"   .= (api_key ev)
      , "ttl"       .= (400 :: Integer)
      , "tags"    .= (tags ev)
      , "metric"  .= (metric ev)
      ]

objectNoNulls :: [AT.Pair] -> Value        
objectNoNulls = object . Prelude.filter (\(_, v) -> v /= Null)


sendHeart :: Request -> Event -> IO ()
sendHeart req body = do 
          withManager $ \manager -> do
              let reqHead = req { 
                  method = "POST",
                  --checkStatus = \_ _  _ -> Nothing,
                  requestBody = RequestBodyBS (L.toStrict $ encode body)
              }                    
              _ <- http reqHead manager -- httpLbs for simpler fetching of body content                                        
              return () --can also be responseHeaders or responseBody etc

type StateString = String
type TimeTaken = Double

notify :: Request -> C.Check -> StateString -> F.StatusCheck -> IO ()
notify heartTarget chk _state sc = do 
        let ev = Event {
            state = Just $ pack _state,
            check = C.check chk,
            summary = Just $ pack $ show sc,
            api_key = C.api_key chk,
            tags = C.tags chk,
            metric = Nothing
        }
        notifyDo heartTarget ev



notifyM :: Request -> C.Check -> M.MetricName -> Maybe M.MetricValue -> IO ()
notifyM heartTarget chk metricName (Just metricVal) = do 
        let ev = Event {
            state = Nothing,
            check = Data.Text.concat [C.check chk, " ", metricName],
            summary = Nothing,
            api_key = C.api_key chk,
            tags = [metricName] ++ C.tags chk,
            metric = Just metricVal
        }
        notifyDo heartTarget ev
notifyM _ _ _ Nothing = return ()         

notifyDo :: Request -> Event -> IO ()
notifyDo heartTarget ev = do 
  result <- E.try $ sendHeart heartTarget ev :: IO (Either HttpException ())
  case result of  
    Left e -> print e
    _ -> return ()        


notifyFor :: Request ->  C.Check -> (F.StatusCheck, Maybe TimeTaken) -> IO ()

{--
 Are droppoing all non metric checks for now. 
 We need a better way of presenting the availability check data to users. 
 So for now - it isn't used. 
--}

notifyFor heartTarget chk (F.OK, _timeTaken) = do
  --MN don't care about uptime, only response time: 
  --notify heartTarget chk "OK" F.OK
  notifyM heartTarget chk "response_time" _timeTaken
  putStrLn $ (C.toUrl chk) ++ " OK"

notifyFor heartTarget chk (F.MetricsChecks mt, _timeTaken) = do  
  --MN/LH - yeah lets not track response time and uptime for metrics:
  --notify heartTarget chk "OK" F.OK
  --notifyM heartTarget chk "response_time" _timeTaken 
  _ <- mapM (\(mName,mVal) -> notifyM heartTarget chk mName (Just mVal)) checkList
  putStrLn $ (show checkList)
  where
    checkList = HM.toList (M.metrics mt)
    
    
-- a catch all for cases where it is dysfunctional. Time taken doesn't make sense at this point.
notifyFor heartTarget chk (bad, _) = do 
  --MN: will ignore this for now: 
  --notify  heartTarget chk "CRITICAL" bad
  putStrLn $ (C.toUrl chk) ++ " CRITICAL (but no action taken) " ++ (show bad) ++ (show heartTarget)
      
    
        
