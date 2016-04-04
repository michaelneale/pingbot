{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Metrics where

import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as L
import qualified StatusioMetrics as SIO

-- This is what is received from the customers endpoint
data MetricsResponse = 
  MetricsResponse {
    metrics :: HM.HashMap MetricName MetricValue
  } deriving (Show, Generic)
instance FromJSON MetricsResponse

parseMetrics :: L.ByteString -> Maybe MetricsResponse
parseMetrics resp = 
  case parseStandard resp of
    Just mr -> Just mr
    Nothing -> 
      case parseSio resp of
        Just mr -> Just $ MetricsResponse (SIO.toHM mr)
        Nothing -> Nothing

parseSio :: L.ByteString -> Maybe SIO.SioMetricsResponse
parseSio resp = decode resp

parseStandard :: L.ByteString -> Maybe MetricsResponse
parseStandard resp = decode resp


{--
-- this will be used by the pingbot
--processMetrics :: C.Check -> MetricsResponse -> IO [LearnResponse]
processMetrics :: Request -> C.Check -> MetricsResponse -> IO [MetricLearnResponse]
processMetrics req chk mr = do
  currentTime <- currentTimeS
  responses <- mapM (\m -> doLearn m req (toLearnReq currentTime m chk)) mList
  return $ catMaybes responses  
  where         
    mList = HM.toList (metrics mr)
--}

type MetricName = Text
type MetricValue = Double
