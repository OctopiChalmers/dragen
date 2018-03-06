{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Countable where

import GHC.Generics
import Countable

----------------------------------------

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

countT1T2 = count (B (B (B A (D (B A C))) (D A)) (D A))
-- ==> fromList [("A",4),("B",4),("C",1),("D",3)]

----------------------------------------

data List
    = Nil
    | Cons () List
    deriving (Show, Generic)

instance Countable List
instance Countable ()

countList = count (Cons () (Cons () Nil))
-- ==> fromList [("()",2),("Cons",2),("Nil",1)]

----------------------------------------

data Tree
    = Leaf
    | Node Tree Tree
    deriving (Show, Generic)

instance Countable Tree

countTree = count (Node (Node Leaf Leaf) (Node Leaf Leaf))
-- ==> fromList [("Leaf",4),("Node",3)]

----------------------------------------

data GTree a
    = GLeaf
    | GNode (GTree a) a (GTree a)
    deriving (Show, Generic1)

deriving instance (Generic a) => Generic (GTree a)
instance (Generic a, Countable a) => Countable (GTree a)

instance Countable Bool

countGTree = count (GNode (GNode GLeaf True GLeaf) False (GNode GLeaf True GLeaf))
-- ==> fromList [("False",1),("GLeaf",4),("GNode",3),("True",2)]

instance Countable1 GTree

count1GTreeInt = count1 (GNode (GNode GLeaf 1    GLeaf) 2     (GNode GLeaf 3    GLeaf))
-- ==> fromList [("GLeaf",4),("GNode",3)]

count1GTreeBool = count (GNode (GNode GLeaf True GLeaf) False (GNode GLeaf True GLeaf))
-- ==> fromList [("GLeaf",4),("GNode",3)]
