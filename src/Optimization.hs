module Optimization where

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map.Strict as Map

import Prediction
import TypeInfo

import System.IO.Unsafe
import System.IO

dot :: a -> a
dot x = unsafePerformIO (putStr "*" >> hFlush stdout >> return x)

epsilon :: Double
epsilon = 0.0001

type CostFunction = TypeEnv -> Size -> FreqMap -> Double

uniform :: CostFunction
uniform env size freqs = chiSquare (fromIntegral size) observed
  where observed = Map.elems (predict env size freqs)

weighted :: [(Name, Int)] -> CostFunction
weighted weights env size freqs = chiSquareVec expected observed
  where
    prediction = predict env size freqs
    (cnames, observed) = unzip (Map.toList (Map.filterWithKey weighted prediction))
    weighted cn cp = isJust (lookup cn weights)
    expected = map multWeight cnames
    multWeight cn = fromIntegral (fromJust (lookup cn weights) * size)

without :: ([Name] -> TypeEnv -> [Name]) -> [Name] -> CostFunction
without f names env size freqs = chiSquareVec expected observed
  where
    prediction = predict env size freqs
    (cnames, observed) = unzip (Map.toList prediction)
    expected = map applyBan cnames
    applyBan cn
      | cn `notElem` f names env = fromIntegral size
      | otherwise = epsilon

only :: ([Name] -> TypeEnv -> [Name]) -> [Name] -> CostFunction
only f names env size freqs = chiSquareVec expected observed
  where
    prediction = predict env size freqs
    (cnames, observed) = unzip (Map.toList prediction)
    expected = map applyBan cnames
    applyBan cn
      | cn `elem` f names env = fromIntegral size
      | otherwise = epsilon

types :: [Name] -> TypeEnv -> [Name]
types ts env = filter ((`elem` ts) . typeName . conType env) (consList env)

constructors :: [Name] -> TypeEnv -> [Name]
constructors names _ = names

--------------------------------------------------------------------------------

chiSquareVec :: (Floating a) => [a] -> [a] -> a
chiSquareVec expected observed
  = sum (zipWith (\o e -> (o - e)^2 / e) observed expected)

chiSquare :: (Floating a) => a -> [a] -> a
chiSquare expected observed = chiSquareVec (repeat expected) observed


--
-- Local search optimization method.
--
type Heat = Double

optimizeLS :: TypeEnv -> Size -> CostFunction -> FreqMap -> FreqMap
optimizeLS env size cost freqs
  = localSearch env size (fromIntegral size ^ 2) cost freqs []

localSearch :: TypeEnv -> Size -> Heat -> CostFunction
            -> FreqMap -> [FreqMap] -> FreqMap
localSearch env size heat cost focus visited
  | null newNeighbors = focus
  | delta <= epsilon && heat == 1 = focus
  | delta <= epsilon
    = dot $ localSearch env size 1 cost bestNeighbor newFrontier
  | otherwise
    = dot $ localSearch env size newHeat cost bestNeighbor newFrontier
  where
    delta = focusCost - bestNeighborCost
    focusCost = cost env size focus
    (bestNeighbor, bestNeighborCost) = minimumBy (comparing snd) neighborsCosts
    neighborsCosts = zip newNeighbors (map (cost env size) newNeighbors)
    newNeighbors = neighborhood focus heat \\ (focus:visited)

    newHeat = max 1 ((heat / (1 + 0.01 * (gainRatio / fromIntegral size))))
    gainRatio = bestNeighborCost / focusCost
    newFrontier = newNeighbors ++ (take (length env ^ 2)) visited

neighborhood :: FreqMap -> Heat -> [FreqMap]
neighborhood freqs heat = map (Map.fromList . zip names) neighborFreqs
  where
    (names, ints) = unzip (Map.toList freqs)
    neighborFreqs = concatMap neighborsAt notBuiltInFreqs

    neighborsAt i = [ updateAt i (ints!!i + floor heat)
                    , updateAt i (max 1 (ints!!i - floor heat)) ]
    updateAt i v = take i ints ++ [v] ++ drop (i+1) ints

    notBuiltInFreqs = filter (not . builtIn) (take (length names) [0..])
    builtIn i = Map.member (names !! i) builtInFreqs

