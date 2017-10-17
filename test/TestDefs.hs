{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StandaloneDeriving #-}

module TestDefs where

import Test.QuickCheck
import Language.Haskell.TH
import GHC.Generics

import Countable
import DeriveArbitrary

deriving instance Generic Int
instance Countable Int
instance Countable a => Countable (Maybe a)

----------------------------------------

data List
    = Nil Int
    | Cons List
    deriving (Show, Generic)

instance Countable List

----------------------------------------

data TreeA
    = LeafA Int
    | NodeA TreeA TreeA
    deriving (Show, Generic)

instance Countable TreeA

----------------------------------------

data TreeB
    = Leaf1B
    | Leaf2B
    | Node2B TreeB TreeB
    | Node3B TreeB TreeB TreeB
    deriving (Show, Generic)

instance Countable TreeB

----------------------------------------

data GTree a
    = GLeaf1
    | GNode2 a (GTree a) (GTree a)
    | GNode3 a a (GTree a) (GTree a) (GTree a)
    deriving (Show, Generic, Generic1)

instance Countable a => Countable (GTree a)

type GTreeBool = GTree Bool

----------------------------------------

data T1 = A Int | B T1 T2 deriving (Show, Generic)
data T2 = C     | D T1    deriving (Show, Generic)

instance Countable T1
instance Countable T2

----------------------------------------

data X = X1 Y | X2 deriving (Show, Generic)
data Y = Y1 Z | Y2 deriving (Show, Generic)
data Z = Z1 X | Z2 deriving (Show, Generic)

instance Countable X
instance Countable Y
instance Countable Z

----------------------------------------

data Option a
    = None
    | Some a
    deriving (Show, Generic, Generic1)

instance Countable1 Option
instance Countable a => Countable (Option a)

----------------------------------------

data Tup = Tup (Int, Char)

----------------------------------------

data ListArg = ListCon [Int]

----------------------------------------

newtype NewType = SingleCon { field :: (Int,Bool) }

----------------------------------------

data BuiltIn = BuiltIn (Maybe Bool) deriving (Show, Generic)

instance Countable Bool
instance Countable BuiltIn

----------------------------------------

data Weighted
  = LeafW1 | LeafW2
  | BranchW Weighted Weighted
  deriving (Show, Generic)

instance Countable Weighted

----------------------------------------

data Weighted'
  = Branch1W' Weighted'
  | Branch2W' Weighted'
  | LeafW'
  deriving (Show, Generic)

instance Countable Weighted'

----------------------------------------

data Rose
  = RLeaf (Maybe Bool)
  | RNode [Rose]
  deriving (Show, Generic)

instance Countable a => Countable [a]
instance Countable Rose

----------------------------------------

type TupIntBool = (Int, Bool)

----------------------------------------

data AA = AB BB | AC CC | ABC BB CC | AA deriving (Show, Generic)
data BB = BA AA | BC CC | BAC AA CC | BB deriving (Show, Generic)
data CC = CA AA | CB BB | CAB AA BB | CC deriving (Show, Generic)

instance Countable AA
instance Countable BB
instance Countable CC

----------------------------------------

data T1' = A' Int | B' [T1'] T2' deriving (Show, Generic)
data T2' = C'     | D' T1'       deriving (Show, Generic)

instance Countable T1'
instance Countable T2'

----------------------------------------

data T1'' = A'' | B'' (Maybe T1'') T2'' deriving (Show, Generic)
data T2'' = C'' | D'' T1''              deriving (Show, Generic)

instance Countable T1''
instance Countable T2''

----------------------------------------

-- deriveArbitrary ''GTreeBool  10 uniform
-- deriveArbitrary ''AA         10 uniform
-- deriveArbitrary ''TreeB      10 uniform

-- deriveArbitrary ''List       10 uniform
-- deriveArbitrary ''TreeA      10 uniform
-- deriveArbitrary ''Tup        10 uniform
-- deriveArbitrary ''NewType    10 uniform
-- deriveArbitrary ''BuiltIn    10 uniform
-- deriveArbitrary ''T1         10 uniform
-- deriveArbitrary ''TupIntBool 10 uniform
-- deriveArbitrary ''Rose       10 uniform
-- deriveArbitrary ''T1'       10 uniform
deriveArbitrary ''T1''       10 uniform

-- deriveArbitrary ''Weighted   10 (weighted [('LeafW1, 10), ('LeafW2, 1)])
-- deriveArbitrary ''Weighted'  10 (weighted [('Branch1W', 5), ('Branch2W', 1)])

-- deriveArbitrary ''AA         10 (only types [''AA, ''CC])
-- deriveArbitrary ''TreeB      10 (only constructors ['Leaf1B, 'Node3B])
-- deriveArbitrary ''GTreeBool  10 (without constructors ['GNode2])

