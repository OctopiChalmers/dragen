{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE TypeOperators  #-}

module Countable where

import GHC.Generics

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

{-
    This file provides a generic implementation for counting how many times a
    type constructor appears in a value. We use two different type classes in
    order to achieve this, called `Countable` (for types of kind *) and
    `Countable1` (for types of kind (* -> *)).

    class Countable (a :: *) where
        count :: a -> ConsMap

    class Countable1 (f :: * -> *) where
        count1 :: f a -> ConsMap

    Let us suppose we have the following type definition:

    data Tree = Leaf | Node Tree Tree

    if we want to count how many times each type constructor appears within a
    given value of type `Tree`, we need to add the following instance
    derivations:

    deriving instance Generic Tree
    instance Countable Tree

    Then, we can count type constructors over values of type `Tree`:

    count (Node (Node Leaf Leaf) (Node Leaf Leaf))
    ==> fromList [("Leaf",4),("Node",3)]

    Note that, if the `Tree` data type definition is available, the `deriving
    instance Generic Tree` could be avoided by including Generic at the
    type definition deriving clause:

    data Tree = Leaf | Node Tree Tree deriving Generic

    `Countable` requires every subtype of a `Countable` data type to be also
    `Countable` in order to work. If we modify our `Tree` definition as

    data GTree a = GLeaf | GNode GTree a GTree

    then is necessary to add the following instance derivations:

    instance deriving Generic a => Generic (GTree a)
    instance (Generic a, Countable a) => Countable (GTree a)

    and the `Generic` and `Countable` derivations for whatever `a` we want to
    use.  For example, let `a` be `Bool` (`Bool` already has a `Generic`
    instance):

    instance Countable Bool

    then we can count type constructors on values of type `GTree Bool`

    count (GNode (GNode GLeaf False GLeaf) True (GNode GLeaf True GLeaf))
    ==> fromList [("False",1),("GLeaf",4),("GNode",3),("True",2)]

    but what if we are just interested in counting `GLeaf` and `GNode`
    type constructors within values of type `GTree a`? Using `Countable` type
    class would require to provide (or derive) proper `Generic` and `Countable`
    instances for whatever type we instantiate `a` with. Fortunately, we can
    define a new type class, `Countable1`, for types of kind (* -> *) that does
    not count type constructors further than the outter data type. Later, we
    derive a `Countable1` instance for `GTree`.

    instance Countable1 GTree

    count1 (GNode (GNode GLeaf 1 GLeaf) 2 (GNode GLeaf 3 GLeaf))
    ==> fromList [("GLeaf",4),("GNode",3)]

    count1 (GNode (GNode GLeaf "a" GLeaf) "b" (GNode GLeaf "c" GLeaf))
    ==> fromList [("GLeaf",4),("GNode",3)]


-}

type ConsMap = Map String Int

class Countable (a :: *) where
    count :: a -> ConsMap

    default count :: (Generic a, GCountable (Rep a)) => a -> ConsMap
    count = gcount . from


class GCountable f where
    gcount :: f a -> ConsMap


instance GCountable (URec a) where
    gcount _ = Map.empty

instance GCountable V1 where
    gcount _ = Map.empty

instance GCountable U1 where
    gcount U1 = Map.empty

instance Countable a => GCountable (K1 i a) where
    gcount (K1 x) = count x

instance (GCountable f, GCountable g) => GCountable (f :*: g) where
    gcount (f :*: g)  = Map.unionWith (+) (gcount f) (gcount g)

instance (GCountable f, GCountable g) => GCountable (f :+: g) where
    gcount (L1 x) = gcount x
    gcount (R1 x) = gcount x

instance (Constructor c, GCountable f) => GCountable (C1 c f) where
    gcount cons@(M1 inner) = Map.unionWith (+)
        (Map.singleton (conName cons) 1) (gcount inner)

instance GCountable f => GCountable (D1 c f) where
    gcount (M1 x) = gcount x

instance GCountable f => GCountable (S1 c f) where
    gcount (M1 x) = gcount x

--------------------------------------------------------------------------------

class Countable1 (f :: * -> *) where
    count1 :: f a -> ConsMap

    default count1 :: (Generic1 f, GCountable1 (Rep1 f)) => f a -> ConsMap
    count1 = gcount1 . from1


class GCountable1 f where
    gcount1 :: f a -> ConsMap


instance GCountable1 V1 where
    gcount1 _ = Map.empty

instance GCountable1 U1 where
    gcount1 U1 = Map.empty

instance GCountable1 Par1 where
    gcount1 (Par1 a) = Map.empty

instance (Countable1 f) => GCountable1 (Rec1 f) where
    gcount1 (Rec1 a) = count1 a

instance (GCountable1 f, GCountable1 g) => GCountable1 (f :*: g) where
    gcount1 (f :*: g)  = Map.unionWith (+) (gcount1 f) (gcount1 g)

instance (GCountable1 f, GCountable1 g) => GCountable1 (f :+: g) where
    gcount1 (L1 x) = gcount1 x
    gcount1 (R1 x) = gcount1 x

instance (Constructor c, GCountable1 f) => GCountable1 (C1 c f) where
    gcount1 cons@(M1 inner) = Map.unionWith (+)
        (Map.singleton (conName cons) 1) (gcount1 inner)

instance GCountable1 f => GCountable1 (D1 c f) where
    gcount1 (M1 x) = gcount1 x

instance GCountable1 f => GCountable1 (S1 c f) where
    gcount1 (M1 x) = gcount1 x
