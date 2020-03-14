{-# LANGUAGE OverloadedStrings #-}

module Metrics where

import qualified Data.Text as T
import           Lib
import           TextShow  (showt)

createMetrics :: [Result] -> T.Text
createMetrics rs = T.unlines (header <> content)
  where
    header = ["# HELP build_status_gauge committed offsets", "# TYPE build_status_gauge gauge"]
    content = convertMetric <$> rs

convertMetric :: Result -> T.Text
convertMetric (Result name status) = mconcat ["build_status_gauge{repository=\"", name, "\"} ", (showt . fromEnum) status]
-- # HELP build_status_gauge committed offsets
-- # TYPE build_status_gauge gauge
-- build_status_gauge{repository="dead.letter",partition="4",} 323.0
-- build_status_gauge{repository="dead.letter",partition="3",} 354.0
-- build_status_gauge{repository="dead.letter",partition="2",} 355.0
-- build_status_gauge{repository="dead.letter",partition="1",} 338.0
-- build_status_gauge{repository="dead.letter",partition="0",} 343.0
