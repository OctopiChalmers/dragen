module TypeInfo
( module Language.Haskell.TH
, module TypeInfo
) where

import Data.List
import Data.Maybe
import Data.Function

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Name)

import Debug.Trace

-- Data types for representing Haskell data type definitions.
data TypeDef = TypeDef
    { tsig :: Type      -- ^ Left hand side of the type definition.
    , tcons :: [Con]    -- ^ Type constructors
    , prim :: Bool      -- ^ Is this data type primitive? (e.g. Int, Bool)
    } deriving Show

data Con = Con
    { cname :: Name     -- ^ Constructor name
    , cargs :: [Type]   -- ^ Constructor args
    , rec :: Bool       -- ^ Is this constructor recursive?
    } deriving Show

data Type
    = Base Name         -- ^ A base type name (e.g. Int, Maybe, Either)
    | Var Name          -- ^ A type variable (e.g. a, b)
    | App Type Type     -- ^ A type application (e.g. T U)
    deriving (Show, Eq, Ord)

instance Eq TypeDef where
    (==) = (==) `on` tsig

apply :: Name -> [Type] -> Type
apply name = foldl App (Base name)

unapply :: Type -> (Name, [Type])
unapply (Base name) = (name, [])
unapply (Var name) = (name, [])
unapply (App l r) = (name, l' ++ [r])
  where (name, l') = unapply l

typeName :: TypeDef -> Name
typeName = fst . unapply . tsig

typeArgs :: TypeDef -> [Type]
typeArgs = snd . unapply . tsig

flatten :: Type -> [Name]
flatten (Base name) = [name]
flatten (Var name) = [name]
flatten (App l r) = flatten l ++ flatten r

subtype :: Type -> Type -> Bool
subtype t t' | t == t' = True
subtype t (App l r) = subtype t l || subtype t r
subtype t t' = False

occurrences :: Type -> Con -> Int 
occurrences ts con = countSat (==ts) (cargs con)

countSat :: (a -> Bool) -> [a] -> Int
countSat p = length . filter p



type TypeEnv = [TypeDef]

-- Extract type signatures from a type env.
typeSigs :: TypeEnv -> [Type]
typeSigs = map tsig

-- Extract the list of type constructor names from a type env.
consList :: TypeEnv -> [Name]
consList env = nub (map cname (concatMap tcons env))

-- Extracts type parameters of every type constructor.
involvedWith :: TypeDef -> [Type]
involvedWith = nub . concatMap cargs . tcons

-- Extracts a type constructor from a given type.
getCon :: Name -> TypeDef -> Con
getCon cn t = fromMaybe
    (error $ "getCon: looking for " ++ show cn ++ " in " ++ show (tsig t))
    (find ((cn==) . cname) (tcons t))

-- Given a type constructor name, finds its associated type.
conType :: TypeEnv -> Name -> TypeDef
conType env cn = fromMaybe
    (error $ "conType: " ++ show cn ++ " not found in " ++ show env)
    (find (any ((cn==) . cname) . tcons) env)

-- Given a type constructor name, returns its siblings (including itself).
getSiblings :: Name -> TypeEnv -> [Con]
getSiblings cn env = tcons (conType env cn)

-- Is a type constructor sibling with another?
isSibling :: TypeEnv -> Name -> Name -> Bool
isSibling env = (==) `on` conType env

-- Split constructors into recursive and terminals.
splitCons :: TypeEnv -> ([Con], [Con])
splitCons = partition rec . concatMap tcons

getRecursives :: TypeEnv -> [Con]
getRecursives = fst . splitCons

getTerminals :: TypeEnv -> [Con]
getTerminals = snd . splitCons

-- Is this constructor terminal?
isTerminal :: TypeEnv -> Name -> Bool
isTerminal env cn = cn `elem` map cname (getTerminals env)
