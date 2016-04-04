{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module StatusioMetrics where

import Data.Text
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Data.Aeson

  
-- This is what is received from the customers endpoint
data SioMetricsResponse = 
  SioMetricsResponse {
    metrics :: [MetricContent]
  } deriving (Show, Generic)

instance FromJSON SioMetricsResponse

data MetricContent = 
  MetricContent {
      metric :: MetricInfo,
      summary :: SummaryInfo
  } deriving (Show, Generic)
instance FromJSON MetricContent  


data MetricInfo = 
  MetricInfo {
    name :: Text
  } deriving (Show, Generic)
instance FromJSON MetricInfo  
  
  
data SummaryInfo = 
    SummaryInfo {
      mean :: Double,
      sum :: Double
    } deriving (Show, Generic)
instance FromJSON SummaryInfo
    
    
toHM :: SioMetricsResponse -> HM.HashMap Text Double
toHM mr = 
  HM.fromList $ Prelude.map (\mc -> (name $ metric mc, mean $ summary mc)) $ metrics mr  
