{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples where

import Test.QuickCheck
import Language.Haskell.TH
import GHC.Generics

import Countable
import Dragen

deriving instance Generic Int
instance Countable Int
instance Countable a => Countable (Maybe a)

--------------------------------------------------------------------------------

--
-- Binary tree with three kinds of leafs
--

data Tree
  = LeafA
  | LeafB
  | LeafC
  | Node Tree Tree
  deriving (Show, Generic)

-- Countable generic typeclass let us count the number of each kind of
-- constructors are present within a value. We automatically derive a type
-- instance for Tree as follows:
instance Countable Tree

-- Now we derive and optimize a random generator for Tree, defining an Arbitrary
-- Tree class instance. The predicted distribution of the derived generator is
-- shown in the derivation process as follows:

dragenArbitrary ''Tree 10 uniform

-- Finally, we can confirm the predicted distribution of this derived generator
-- sampling a big number of values, and averaging the number of generated
-- constructors of each kind of type constructor:
confirmTree :: IO ()
confirmTree = confirm 10 (arbitrary :: Gen Tree)
-- =====>
--  * ("LeafA",5.23938)
--  * ("LeafB",5.23396)
--  * ("LeafC",5.18283)
--  * ("Node",14.65617)

--------------------------------------------------------------------------------

--
-- Mutually recursive data types
--

data T1
  = A
  | B T1 T2
  deriving (Show, Generic)

data T2
  = C
  | D T1
  deriving (Show, Generic)

instance Countable T1
instance Countable T2

dragenArbitrary ''T1 10 uniform

confirmT1 = confirm 10 (arbitrary :: Gen T1)
-- =====>
--  * ("A",8.63757)
--  * ("B",13.67604)
--  * ("C",6.03847)
--  * ("D",7.63757)

--------------------------------------------------------------------------------

--
-- Rose trees (mutually recursive between Rose and [] data types) with composite
-- types Maybe and Bool
--

data Rose
  = RLeaf (Maybe Bool)
  | RNode [Rose]
  deriving (Show, Generic)

instance Countable a => Countable [a]
instance Countable Rose
instance Countable Bool

dragenArbitrary ''Rose 10 uniform

confirmRose = confirm 10 (arbitrary :: Gen Rose)
-- =====>
--  * (":",16.61068)
--  * ("False",4.17641)
--  * ("Just",8.35818)
--  * ("Nothing",2.79404)
--  * ("RLeaf",11.15222)
--  * ("RNode",6.45846)
--  * ("True",4.18177)
--  * ("[]",6.45846)

--------------------------------------------------------------------------------

--
-- Weighted generation.
--

data Tree'
  = Leaf
  | NodeA Tree' Tree'
  | NodeB Tree' Tree'
  deriving (Show, Generic)

instance Countable Tree'

dragenArbitrary ''Tree' 10 (weighted [('NodeA, 3), ('NodeB, 1)])

confirmTree' = confirm 10 (arbitrary :: Gen Tree')
-- =====>
--  * ("Leaf",41.11079)
--  * ("NodeA",30.47836)
--  * ("NodeB",9.63243)

--------------------------------------------------------------------------------

--
-- Lambda expressions with weighted generation
--

data Expr
  = Var Char
  | App Expr Expr
  | Lam Char Expr
  deriving (Show, Generic)

deriving instance Generic Char
instance Countable Char
instance Countable Expr

dragenArbitrary ''Expr 10 (weighted [('Var, 3), ('Lam, 1)])

confirmExpr = confirm 10 (arbitrary :: Gen Expr)
-- =====>
--  * ("App",29.15031)
--  * ("C#",40.21184)   (boxed char)
--  * ("Lam",10.06153)
--  * ("Var",30.15031)

--------------------------------------------------------------------------------

--
-- Lisp expressions used in the paper presentation
--

data Text 
  = Text
  deriving (Show, Generic)

data Lisp
  = Symbol Text
  | String Text
  | Number Int
  | List [Lisp]
  deriving (Show, Generic)

instance Countable Text
instance Countable Lisp

dragenArbitrary ''Lisp 10 uniform

confirmLisp = confirm 10 (arbitrary :: Gen Lisp)
-- =====>
-- * (GHC.Types.:,17.838586498464217)
-- * (GHC.Types.[],7.0449940565497196)
-- * (Examples.List,7.0449940565497196)
-- * (Examples.Number,5.018549975282765)
-- * (Examples.String,3.3875212333158666)
-- * (Examples.Symbol,3.3875212333158666)
-- * (Examples.Text,6.775042466631733)
-- * (GHC.Types.Int,5.018549975282765)

