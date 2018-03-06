{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples where

import Test.QuickCheck
import Language.Haskell.TH
import GHC.Generics

import Countable
import DeriveArbitrary

deriving instance Generic Int
instance Countable Int
instance Countable a => Countable (Maybe a)


--
-- This file automatically derives generators for examples shown in the article,
-- plus for some other interesting data types.
--


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

-- Predicted distribution for the optimized frequencies map:
--  * (Examples.LeafA,5.261312269824188)
--  * (Examples.LeafB,5.261312269824188)
--  * (Examples.LeafC,5.214751984250523)
--  * (Examples.Node,14.737376523898902)
-- Cost: 9.025165770966032

-- Along with the synthetized code (requires flag -ddump-splices)

-- Deriving optimized generator...
-- Visiting:Examples.Tree
-- test/Examples.hs:38:1-33: Splicing declarations
--     deriveArbitrary ''Tree 10 uniform
--   ======>
--     instance Arbitrary Tree where
--       arbitrary
--         = sized go_alF8
--         where
--             go_alF8 n_alF9
--               = if (n_alF9 == 0) then
--                     frequency
--                       [ (113, return LeafA)
--                       , (113, return LeafB)
--                       , (112, return LeafC) ]
--                 else
--                     frequency
--                       [ (113, return LeafA)
--                       , (113, return LeafB)
--                       , (112, return LeafC)
--                       , (497, 
--                           Node <$> go_alF8 ((max 0) (n_alF9 - 1))
--                             <*> go_alF8 ((max 0) (n_alF9 - 1))) ]
--
deriveArbitrary ''Tree 10 uniform

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

deriveArbitrary ''T1 10 uniform

-- Predicted distribution for the optimized frequencies map:
--  * (Examples.A,8.667025787257028)
--  * (Examples.B,13.725195277382783)
--  * (Examples.C,6.058169490125756)
--  * (Examples.D,7.6670257872570255)
-- Cost: 3.6634696552371873

-- Deriving optimized generator...
-- Visiting:Examples.T1
-- Visiting:Examples.T2
-- test/Examples.hs:106:1-31: Splicing declarations
--     deriveArbitrary ''T1 10 uniform
--   ======>
--     instance Arbitrary T2 where
--       arbitrary
--         = sized go_a1ing
--         where
--             go_a1ing n_a1inh
--               = if (n_a1inh == 0) then
--                     return C
--                 else
--                     frequency
--                       [(89, return C),
--                        (199, D <$> (resize ((max 0) (n_a1inh - 1))) arbitrary)]
--     instance Arbitrary T1 where
--       arbitrary
--         = sized go_a1ini
--         where
--             go_a1ini n_a1inj
--               = if (n_a1inj == 0) then
--                     return A
--                 else
--                     frequency
--                       [(70, return A),
--                        (213, 
--                         B <$> go_a1ini ((max 0) (n_a1inj - 1))
--                           <*> (resize ((max 0) (n_a1inj - 1))) arbitrary)]

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

deriveArbitrary ''Rose 10 uniform

-- Predicted distribution for the optimized frequencies map:
--  * (GHC.Base.Just,8.453368179037742)
--  * (GHC.Base.Nothing,2.6948656946596827)
--  * (GHC.Types.:,16.603763000012577)
--  * (GHC.Types.False,4.226684089518871)
--  * (GHC.Types.True,4.226684089518871)
--  * (GHC.Types.[],6.45552912631515)
--  * (Examples.RLeaf,11.148233873697425)
--  * (Examples.RNode,6.455529126315149)
-- Cost: 19.24740847509439

-- Deriving optimized generator...
-- Visiting:Examples.Rose
-- test/Examples.hs:152:1-33: Splicing declarations
--     deriveArbitrary ''Rose 10 uniform
--   ======>
--     instance Arbitrary Rose where
--       arbitrary
--         = sized go_a1itH
--         where
--             go_a1itH n_a1itI
--               = if (n_a1itI == 0) then
--                     RLeaf <$> arbitrary
--                 else
--                     frequency
--                       [(100, RLeaf <$> arbitrary),
--                        (100, 
  --                         RNode 
--                           <$>
--                             (resize ((max 0) (n_a1itI - 1)))
--                               ((Arbitrary.customListGen 1) 397))]

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

deriveArbitrary ''Tree' 10 (weighted [('NodeA, 3), ('NodeB, 1)])

-- Predicted distribution for the optimized frequencies map:
--  * (Examples.Leaf,41.13723849786045)
--  * (Examples.NodeA,30.491855603025765)
--  * (Examples.NodeB,9.64538289483468)
-- Cost: 2.0639393601844427e-2

-- Deriving optimized generator...
-- Visiting:Examples.Tree'
-- test/Examples.hs:234:1-64: Splicing declarations
--     deriveArbitrary ''Tree' 10 (weighted [('NodeA, 3), ('NodeB, 1)])
--   ======>
--     instance Arbitrary Tree' where
--       arbitrary
--         = sized go_a1q4V
--         where
--             go_a1q4V n_a1q4W
--               = if (n_a1q4W == 0) then
--                     return Leaf
--                 else
--                     frequency
--                       [(60, return Leaf),
--                        (98, 
--                         NodeA <$> go_a1q4V ((max 0) (n_a1q4W - 1))
--                           <*> go_a1q4V ((max 0) (n_a1q4W - 1))),
--                        (31, 
--                         NodeB <$> go_a1q4V ((max 0) (n_a1q4W - 1))
--                           <*> go_a1q4V ((max 0) (n_a1q4W - 1)))]

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

deriveArbitrary ''Expr 10 (weighted [('Var, 3), ('Lam, 1)])

-- Predicted distribution for the optimized frequencies map:
--  * (Examples.App,29.052261952579133)
--  * (Examples.Lam,10.0230303736398)
--  * (Examples.Var,30.05226195257913)
--  * (GHC.Types.Char,40.07529232621893)
-- Cost: 1.4408353391165505e-4

-- Deriving optimized generator...
-- Visiting:Examples.Expr
-- test/Examples.hs:287:1-59: Splicing declarations
--     deriveArbitrary ''Expr 10 (weighted [('Var, 3), ('Lam, 1)])
--   ======>
--     instance Arbitrary Expr where
--       arbitrary
--         = sized go_a2qTc
--         where
--             go_a2qTc n_a2qTd
--               = if (n_a2qTd == 0) then
--                     Var <$> arbitrary
--                 else
--                     frequency
--                       [(82, Var <$> arbitrary),
--                        (200, 
--                         App <$> go_a2qTc ((max 0) (n_a2qTd - 1))
--                           <*> go_a2qTc ((max 0) (n_a2qTd - 1))),
--                        (69, 
--                         Lam <$> (resize ((max 0) (n_a2qTd - 1))) arbitrary
--                           <*> go_a2qTc ((max 0) (n_a2qTd - 1)))]

confirmExpr = confirm 10 (arbitrary :: Gen Expr)

-- =====>
--  * ("App",29.15031)
--  * ("C#",40.21184)   (boxed char)
--  * ("Lam",10.06153)
--  * ("Var",30.15031)

