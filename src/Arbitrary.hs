{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Arbitrary where

import Data.Maybe
import Data.List
import Data.Map.Strict ((!)) 
import qualified Data.Map.Strict as Map

import Test.QuickCheck
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH

import Megadeth
import TypeInfo
import Prediction


customListGen :: Arbitrary t => Int -> Int -> Gen [t]
customListGen fnil fcons = sized go
  where
    go 0 = return []
    go n = frequency
      [ (fnil, return [])
      , (fcons, (:) <$> resize (n-1) arbitrary <*> go (n-1)) ]

customMaybeGen :: Arbitrary t => Int -> Int -> Gen (Maybe t)
customMaybeGen fnothing fjust = sized go
  where
    go 0 = return Nothing
    go n = frequency
      [ (fnothing, return Nothing)
      , (fjust, Just <$> resize (n-1) arbitrary)
      ]

chooseExpQ :: FreqMap -> Name -> Name -> Name -> Bool -> TH.Type -> ExpQ
chooseExpQ freqs goName nName target recursive ty
  | not recursive
    = [| arbitrary |]
  | headOf ty == target
    = [| $(varE goName) (max 0 ($(varE nName) - 1)) |]
     -- For the case of lists we use a custom generator, since its Arbitrary
     -- instance does not preserve generation frequencies on each iteration.
     -- Dirty hack to dodge Haskell's typeclasses system. This should be
     -- generalized to any type having this issue.
  | headOf ty == ''[]
    = let (fnil, fcons) = (freqs ! '[], freqs ! '(:)) in
      [| resize (max 0 ($(varE nName) - 1)) (customListGen fnil fcons) |]
     -- We need to do this hack for Maybe too. Clearly, this does not scale at
     -- all and we should refactor the code making it independent of the
     -- Arbitrary typeclass.
  | headOf ty == ''Maybe
    = let (fnothing, fjust) = (freqs ! 'Nothing, freqs ! 'Just) in
      [| resize (max 0 ($(varE nName) - 1)) (customMaybeGen fnothing fjust) |]
  | otherwise
    = [| resize (max 0 ($(varE nName) - 1)) arbitrary |]


makeArbExpsQ :: FreqMap -> Name -> Name -> Name ->  [ConView] -> [ExpQ]
makeArbExpsQ freqs goName nName targetName cons
  = map (fmap fixAppl)
    [ foldl (applyTParam rec) (conE conName) conArgs
    | SimpleCon conName rec conArgs <- cons ]
  where
    applyTParam rec rem param = rem `infixAppE` (chooseExp rec param)
    chooseExp rec = chooseExpQ freqs goName nName targetName rec
    infixAppE l r = uInfixE l (varE '(<*>)) r

frequencyExpQ :: FreqMap -> Name -> Name -> Name ->  [ConView] -> ExpQ
frequencyExpQ freqs goName nName target cons
  = [| frequency $(listE tuples) |]
  where
    tuples = map (\(f,g) -> tupE [f,g]) (zip freqExpsQ arbExpsQ)
    freqExpsQ = map getFreqExpQ cons
    arbExpsQ = makeArbExpsQ freqs goName nName target cons
    getFreqExpQ con = maybe [|1|] (\f->[|f|]) (Map.lookup (nm con) freqs)


genTupleArbs :: Int -> ExpQ
genTupleArbs n = doE $
  map (\x -> bindS (varP x) (varE 'arbitrary)) vars ++
  [ noBindS $ appE (varE 'return) (tupE (map varE vars))]
    where vars = take n varNames

isMutRec :: TypeEnv -> ConView -> Bool
isMutRec env con = nm con `elem` recs
  where recs = map cname (getRecursives env)

updateMutRec :: TypeEnv -> ConView -> ConView
updateMutRec env con
    | isMutRec env con = con { recursive = True }
    | otherwise = con


deriveArbitraryInstance :: TypeEnv -> FreqMap -> Name -> Q [Dec]
deriveArbitraryInstance env freqs target = reify target >>= \case

  {- data T {...} = C1 {...} | C2 {...} | ... -}
  TyConI (DataD _ _ params _ cons _) -> do
    let paramExps = map varT (paramNames params)
        allCons = map (updateMutRec env . simpleConView target) cons
        (recCons, termCons) = partition recursive allCons

        mkGo goName nName
          | length allCons == 1
            = head (makeArbExpsQ freqs goName nName target allCons)
          | length recCons == length allCons
            = frequencyExpQ freqs goName nName target recCons
          | length termCons == 1
            = condE [| $(varE nName) == 0 |]
                (head (makeArbExpsQ freqs goName nName target termCons))
                (frequencyExpQ freqs goName nName target allCons)
          | otherwise
            = condE [| $(varE nName) == 0 |]
                (frequencyExpQ freqs goName nName target termCons)
                (frequencyExpQ freqs goName nName target allCons)

    if not (null paramExps)
    then
      [d|
      instance $(applyTo (tupleT (length paramExps))
                         (map (appT (conT ''Arbitrary)) paramExps))
            => Arbitrary $(applyTo (conT target) paramExps) where
        arbitrary = sized go
          where
            go n = $(mkGo 'go 'n)
      |]
    else
      [d|
      instance Arbitrary $(applyTo (conT target) paramExps) where
        arbitrary = sized go
          where
            go n = $(mkGo 'go 'n)
      |]


  {- newtype T {...} = SingleCon {...} -}
  TyConI (NewtypeD _ _ params _ con _) -> do
    let paramExps = map varT (paramNames params)
        singleCon = simpleConView target con

    if not (null paramExps)
    then
      [d|
      instance $(applyTo (tupleT (length paramExps))
                         (map (appT (conT ''Arbitrary)) paramExps))
            => Arbitrary $(applyTo (conT target) paramExps) where
        arbitrary = sized go
          where go n = $(head (makeArbExpsQ freqs 'go 'n target [singleCon]))
      |]
    else
      [d|
      instance Arbitrary $(applyTo (conT target) paramExps) where
        arbitrary = sized go
          where
            go n = $(head (makeArbExpsQ freqs 'go 'n target [singleCon]))
      |]


  {- type T {...} = U {...} -}
  TyConI (TySynD _ params ty) ->
    case (getTy ty) of

      {- type T {...} = ({...}, {...}, ...) -}
      (TupleT n) -> do
        let paramExps = map varT (paramNames params)

        if not (null paramExps)
        then
          [d|
          instance $(applyTo (tupleT (length paramExps))
                             (map (appT (conT ''Arbitrary)) paramExps))
                => Arbitrary $(applyTo (conT target) paramExps) where
            arbitrary = $(genTupleArbs n)
          |]
        else
          [d|
          instance Arbitrary $(applyTo (conT target) paramExps) where
            arbitrary = $(genTupleArbs n)
          |]

      -- This type should had been derived already, It is clearly a
      -- dependency and it should be put before in the topsort.
      (ConT n) -> return []

      _ -> runIO (putStrLn ("IGNORING: " ++ show ty)) >> return []


  {- Int#, Bool#, ...  -}
  PrimTyConI {} -> return []


  {- Not supported yet. ([], (,), ...) -}
  x -> error ("Case not defined: " ++ show x)


devArbitrary :: TypeEnv -> FreqMap -> Name -> Q [Dec]
devArbitrary env freqs
  = megaderive (deriveArbitraryInstance env freqs) (isInsName ''Arbitrary)
