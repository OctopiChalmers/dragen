{-# LANGUAGE BangPatterns #-}

module Dragen
( dragenArbitrary
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


-- | Derives an Abitrary instance for the type `target`, optimizing each type
-- constructor frequency in order to minimize the output of a given cost
-- function.
dragenArbitrary  :: Name -> Size -> CostFunction -> DecsQ
dragenArbitrary target size cost = do

  let putStrLnQ = runIO . putStrLn

  putStrLnQ $ "\nReifiying: " ++ show target

  targetEnv <- reifyNameEnv target

  putStrLnQ $ "\nTypes involved with " ++ show target ++ ":"
  -- putStrLnQ $ show (map tsig targetEnv)
  mapM_ (putStrLnQ . show) targetEnv

  let !freqMap = initMap targetEnv
      !prediction = predict targetEnv size freqMap
      !initCost = cost targetEnv size freqMap

  putStrLnQ $ "\nInitial frequencies map:"
  putStrLnQ $ showMap freqMap
  putStrLnQ $ "\nPredicted distribution for the initial frequencies map:"
  putStrLnQ $ showMap prediction

  putStrLnQ $ "\nOptimizing the frequencies map:"
  let !optimized = optimizeLS targetEnv size cost freqMap
      !prediction' = predict targetEnv size optimized
      !finalCost = cost targetEnv size optimized

  putStrLnQ $ "\n\nOptimized frequencies map:"
  putStrLnQ $ showMap optimized
  putStrLnQ $ "\nPredicted distribution for the optimized frequencies map:"
  putStrLnQ $ showMap prediction'

  putStrLnQ $ "\nInitial cost: " ++ show initCost
  putStrLnQ $ "Final cost: " ++ show finalCost
  putStrLnQ $ "Optimization ratio: " ++ show (initCost / finalCost)

  putStrLnQ $ "\nDeriving optimized generator..."
  devArbitrary targetEnv optimized target
