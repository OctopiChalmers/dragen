{-# LANGUAGE TemplateHaskell #-}

module Prediction where

import Data.Maybe
import Data.Either
import Data.Graph
import Data.List hiding (transpose)
import Data.Matrix hiding ((!), trace)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Test.QuickCheck

import TypeInfo
import Countable

import Debug.Trace

type Size = Int

-- A FreqMap is a mapping between type constructor names and Int values
-- representing the frequencies we want each one of them to occur in a random
-- generated value. This mapping is later provided to QuickCheck `frequency` in
-- order to derivate random value generators.
type FreqMap = Map Name Int

-- Hardcoded instances distributions. This does not work very well, since even
-- if we hardcode the frequencies, the built-in instances does not reduce the
-- size of the inner generation. The solution requires to get rid of the
-- Arbitrary instances and carry arround concrete generators.
builtInFreqs :: FreqMap
builtInFreqs = Map.fromList
  [ {- Bool-}      ('True, 1), ('False, 1)
  , {- Either -}   ('Left, 1), ('Right, 1)
  ]


initMap :: TypeEnv -> FreqMap
initMap = Map.fromList . map setInitialFreq . consList
  where setInitialFreq cn
          | Map.member cn builtInFreqs = (cn, builtInFreqs ! cn * 100)
          | otherwise = (cn, 100)


-- A ProbMap is similar to a FreqMap in the sense that it represents types
-- constructor names vs. frequencies. The difference lie in the fact that for
-- every data type T = C1 .. | C2 .. | ... | Cn then it must hold that
-- pC1 + pC2 + ... + pCn = 1.
type ProbMap = Map Name Double


showMap :: (Show a, Show b) => Map a b -> String
showMap m = intercalate "\n" (map showElem (Map.toList m))
  where showElem e = " * " ++ show e

filterKeys :: (Name -> Bool) -> Map Name b -> Map Name b
filterKeys f = Map.filterWithKey (const . f)



normalize :: TypeEnv -> FreqMap -> ProbMap
normalize env freqMap = Map.mapWithKey freqRatio freqMap
  where
    freqRatio cn cfreq = fromIntegral cfreq / fromIntegral (freqSum cn)
    freqSum cn = Map.foldr (+) 0 (filterKeys (isSibling env cn) freqMap)


normalizeTerminals :: TypeEnv -> FreqMap -> ProbMap
normalizeTerminals env freqMap = Map.mapWithKey freqRatio terminalsMap
  where
    terminalsMap = filterKeys (isTerminal env) freqMap
    freqRatio cn cfreq = fromIntegral cfreq / fromIntegral (freqSum cn)
    freqSum cn = Map.foldr (+) 0 (filterKeys (isSibling env cn) terminalsMap)


genGWMatrix :: TypeEnv -> ProbMap -> Matrix Double
genGWMatrix env probMap = matrix size size genElem
  where
    size = length env
    genElem (m, n) = sum $ map multProb $ occsFromTo (env!!(m-1)) (env!!(n-1))
    multProb (cn, occs) = probMap ! cn * fromIntegral occs
    occsFromTo from to = map (conOccurrences to) (tcons from)
    conOccurrences to con = (cname con, occurrences (tsig to) con)


-- Predicts the distribution for a given type constructor frequencies map.
predict :: TypeEnv -> Size -> FreqMap -> ProbMap
predict env size freqs = prediction
  where

    -- Normalize the frequencies into probabilities
    allProbs  = normalize env freqs
    termProbs = normalizeTerminals env freqs

    -- Split the type environment into branching types and leaf types.
    rootType = env !! 0
    isBranchingType t = t == rootType || any rec (tcons t)
    (branchingTypes, leafTypes) = partition isBranchingType env

    -- Helpers
    bct = tsig . conType branchingTypes     -- branching constructor type
    lct = tsig . conType leafTypes          -- simple constructor type
    m !$ cn = Map.findWithDefault 0 cn m    -- safe lookup

    ----------------------------------------------------------------------------
    -- First we need to calculate the expectancy of the types involved at the
    -- branching process, we do this by calculating the pure random generation
    -- process at the first (size-1) levels, and adding the expectancy of the
    -- pseudo-random generation of the last level.
    ----------------------------------------------------------------------------
    branchingTypesExp = Map.unionWith (+) brFirstLevels brLastLevel

    branchingProbs = filterKeys isBranchingTypeCon allProbs
    branchingTermProbs = filterKeys isBranchingTypeCon termProbs

    isBranchingTypeCon cn = cn `elem` consList branchingTypes
    branchingSigs = typeSigs branchingTypes

    -- Generate the Galton-Watson matrix with the given branching probabilities.
    mT  = genGWMatrix branchingTypes branchingProbs
    ez0 = fromList 1 (length branchingTypes) (1 : repeat 0)

    genLevel 0 = ez0
    genLevel k = ez0 * (mT^k)

    {- Branching process @ first (size-1) levels -}
    brFirstLevels = Map.mapWithKey multTypeExp branchingProbs
      where
        multTypeExp cn cp
          -- Is safe to use the geometric series simplification formula
          | length branchingTypes == 1 && mT' /= 1
            = cp * ((1 - mT' ^ size) / (1 - mT'))
          -- Otherwise, we need to sum every level :(
          | otherwise = cp * typeExp ! bct cn

        mT' = getElem 1 1 mT
        typeExp = Map.fromList $ zip branchingSigs (toList predMatrix)
        predMatrix = foldr1 (+) (map genLevel [0..size-1])


    {- Branching process @ last level -}
    brLastLevel = Map.mapWithKey sumTermExp branchingTermProbs
      where
        sumTermExp tn tp
          = sum [ tp
                * allProbs ! cname con
                * fromIntegral (occurrences (bct tn) con)
                * prevLvlExp ! bct (cname con)
                | con <- concatMap tcons branchingTypes ]

        prevLvlExp = Map.fromList $ zip branchingSigs (toList (genLevel (size-1)))


    ----------------------------------------------------------------------------
    -- Once we have the expectancy for every type constructor involved at the
    -- branching process, we can incorporate the expectancy of the leaf types by
    -- counting how many times they are generated as result of the branching
    -- process. It is important to note here that a leaf type could generate
    -- another leaf type, so we need to perform a topological sort in order to
    -- start calculating the expectancy of the 'nearest' types to the branching
    -- process ones. This way the farthest ones are not multiplied by zero.
    ----------------------------------------------------------------------------
    prediction = addLeafTypesExp branchingTypesExp sortedLeafTypeCons

    addLeafTypesExp pred [] = pred
    addLeafTypesExp pred (cn:cns)
      = addLeafTypesExp (Map.insert cn (sumOccurrences pred cn) pred) cns

    sumOccurrences pred cn
      = sum [ allProbs ! cn
            * pred !$ cname con
            * fromIntegral (occurrences (lct cn) con)
            | con <- allCons ]

    allCons = concatMap tcons env
    leafTypeCons = concatMap tcons leafTypes

    generatorsOf cn = [ cname con | con <- allCons, any (== lct cn) (cargs con) ]

    sortedLeafTypeCons = reverse (map (extractCName . gvert) (topSort graph))
    (graph, gvert) = graphFromEdges' leafTypeDeps
    leafTypeDeps = map createVertex leafTypeCons
    extractCName (_, cn, _) = cn
    createVertex con = ((), cname con, generatorsOf (cname con))



-- Generates a bunch of samples using a given generator and prints the average
-- number of type constructors generated in a random sample of size n. The
-- _arb_ parameter needs a type annotation when the function is used with
-- _arbitraty_ in order to break the ambiguity.
-- E.g.  confirm 10 (arbitrary @Tree)
confirm :: (Countable a) => Size -> Gen a -> IO ()
confirm size arb = do
  let samples = 100000
  values <- sequence (replicate samples (generate (resize size arb)))
  let consCount = Map.unionsWith (+) (map count values)
      consAvg = Map.map (\c -> fromIntegral c / fromIntegral samples) consCount
  putStrLn (showMap consAvg)
