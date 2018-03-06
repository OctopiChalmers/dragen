{-# LANGUAGE BangPatterns #-}

module DeriveArbitrary
( deriveArbitrary
, Optimization.uniform
, Optimization.weighted
, Optimization.only
, Optimization.without
, Optimization.types
, Optimization.constructors
, Prediction.confirm
) where

import Language.Haskell.TH

import Reification
import TypeInfo
import Prediction
import Optimization
import Arbitrary


putStrLnQ :: String -> Q ()
putStrLnQ = runIO . putStrLn


-- Derives an Abitrary instance for the type _target_, optimizing each type
-- constructor frequency in order to minimize the output of a given cost
-- function.
deriveArbitrary  :: Name -> Size -> CostFunction -> DecsQ
deriveArbitrary target size cost = do

  putStrLnQ $ "\nReifiying: " ++ show target

  targetEnv <- reifyNameEnv target

  putStrLnQ $ "\nTypes involved with " ++ show target ++ ":"
  putStrLnQ $ show (map tsig targetEnv)

  let !freqMap = initMap targetEnv
      !prediction = predict targetEnv size freqMap

  putStrLnQ $ "\nInitial frequencies map:"
  putStrLnQ $ showMap freqMap
  putStrLnQ $ "\nPredicted distribution for the initial frequencies map:"
  putStrLnQ $ showMap prediction
  putStrLnQ $ "Cost: " ++ show (cost targetEnv size freqMap)

  putStrLnQ $ "\nOptimizing the frequencies map:"
  let !optimized = optimizeLS targetEnv size cost freqMap
      !prediction' = predict targetEnv size optimized

  putStrLnQ $ "\n\nOptimized frequencies map:"
  putStrLnQ $ showMap optimized
  putStrLnQ $ "\nPredicted distribution for the optimized frequencies map:"
  putStrLnQ $ showMap prediction'
  putStrLnQ $ "Cost: " ++ show (cost targetEnv size optimized)

  putStrLnQ $ "\nDeriving optimized generator..."
  devArbitrary targetEnv optimized target
