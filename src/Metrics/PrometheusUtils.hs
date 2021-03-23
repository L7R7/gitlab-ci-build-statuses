{-# LANGUAGE NoImplicitPrelude #-}

module Metrics.PrometheusUtils (VectorWithLabel (..)) where

import Prometheus

data VectorWithLabel l m = VectorWithLabel (Vector l m) l

instance (Label l, Observer m) => Observer (VectorWithLabel l m) where
  observe (VectorWithLabel vctr label) value = withLabel vctr label f
    where
      f metric = observe metric value
