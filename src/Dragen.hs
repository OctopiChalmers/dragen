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
--
-- IMPORTANT: This function must be called with non-parametric (@kind ~ *@)
-- type names, since we can not resolve the implicit type vars. To reify fully
-- instantiated parametric types, first define a non-parametric type synonym of
-- the target (e.g. @type MaybeInt = Maybe Int@).
--
--  @
--  data Parametric param = ...
--
--  -- NG: Can't resolve the type parameter!
--  $(dragenArbitrary ''Parametric 10 uniform)
--
--  -- OK: Use type synonym by applying to a concrete type
--  type ParametricInt = Parametric Int
--  $(dragenArbitrary ''ParametricInt 10 uniform)
--  @
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
