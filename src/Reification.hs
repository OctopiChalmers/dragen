{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Reification where

import Data.List
import Control.Monad.Extra

import Language.Haskell.TH hiding (Type, Con, prim)
import qualified Language.Haskell.TH as TH

import Megadeth
import TypeInfo


-- Add some info to unsupported features error messages.
unsupported :: Show value => String -> value -> a
unsupported fun input = error $ fun ++ ": unsupported input: " ++ show input

-- Given a type name, extracts its information and the information of the types
-- involved with it from the compiling environment. This functions performs a
-- reachability analysis, looking for mutually recursive type definitions, and
-- marks the _rec_ field of each type constructor accordingly.
-- IMPORTANT: This function should be called with non-parametric (kind ~ *)
-- type names, since we can not resolve the implicit type vars. To reify fully
-- instantiated parametric types, first define a non-parametric type synonym of
-- the target (e.g. type MaybeInt = Maybe Int), or use _reifyTypeEnv_ instead.
reifyNameEnv :: Name -> Q TypeEnv
reifyNameEnv = reifyName >=> reifyInvolvedTypes

-- Similar to _reifyTypeEnv_, but allows us to reify parametric type
-- definitions without needing to define an ad-hoc non-parametric type synonym.
-- E.g. reifyNameEnv ''MaybeInt == reifyTypeEnv (Base ''Maybe `App` Base ''Int)
reifyTypeEnv :: Type -> Q TypeEnv
reifyTypeEnv = reifyType >=> reifyInvolvedTypes



-- Given a type name, extracts its information from the compiling environment.
-- Note this function does not mark the type constructors as mutually recursive
-- if they are mutually recursive with any other type.
-- IMPORTANT: This function should be called with non-parametric (kind ~ *)
--            type names, since we can not resolve the implicit type vars.
reifyName :: Name -> Q TypeDef
reifyName name = reifyType (Base name)


-- Given a type, reifies the leftmost type constructor, and instantiates its
-- type vars with the types applied to the original type.
-- E.g.  reifyType (Base ''Maybe `App` Base ''Int)
--       ===>
--       TypeDef { tsig = App (Base ''Maybe) (Base ''Int)
--               , tcons = [ Con { cname = 'Nothing, ... }
--                         , Con { cname = 'Just, cargs = [Base ''Int], ...} ]
--               , ... }
reifyType :: Type -> Q TypeDef
reifyType this = do

  let (name, args) = unapply this
  reify name >>= \case

    {- data T {...} = C1 {...} | C2 {...} | ... -}
    TyConI (DataD _ _ vars _ cons _) -> do
      let vars' = map extractTVar vars
          binds = zip vars' args
          cons' = map (extractCon binds this) cons
          fields = concatMap (concatMap flatten . cargs) cons'
          emptyCon = Con { cname = name, cargs = [], rec = False }

      primitive <- any isPrim <$> mapM reify fields

      if primitive
      then return
        TypeDef { tsig = apply name [], tcons = [emptyCon], prim = True }
      else return
        TypeDef { tsig = this, tcons = cons', prim = False }

    {- newtype T {...} = Con T' -}
    TyConI (NewtypeD _ _ vars _ con _ ) -> do
      let vars' = map extractTVar vars
          binds = zip vars' args
          con' = extractCon binds this con
          fields = concatMap flatten (cargs con')
          emptyCon = Con { cname = name, cargs = [], rec = False }

      primitive <- any isPrim <$> mapM reify fields

      if primitive
      then return
        TypeDef { tsig = apply name [], tcons = [emptyCon], prim = True }
      else return
        TypeDef { tsig = this, tcons = [con'], prim = False }

    {- type T {...} = U {...} -}
    TyConI (TySynD _ vars ty) -> do
      let vars' = map extractTVar vars
          binds = zip vars' args
          realT = instantiate binds (extractType ty)
      realDef <- reifyType realT
      return realDef { tsig = this }

    {- Int#, Bool#, ...  -}
    PrimTyConI {} -> return
        TypeDef { tsig = apply name [], tcons = [], prim = True }

    {- Not supported yet. -}
    x -> unsupported "reifyType" x


extractTVar :: TyVarBndr -> Name
extractTVar (PlainTV tv) = tv
extractTVar (KindedTV tv _) = tv

extractCon :: [(Name, Type)] -> Type -> TH.Con -> Con
extractCon binds this (NormalC cn cas)
  = Con { cname = cn, cargs = args, rec = this `elem` args }
  where args = map (instantiate binds . extractType . snd) cas
extractCon binds this (InfixC lt op rt)
  = Con { cname = op, cargs = args, rec = this `elem` args }
  where args = map (instantiate binds . extractType . snd) [lt,rt]
extractCon binds this (RecC cn vbts)
  = Con { cname = cn, cargs = args, rec = this `elem` args }
  where args = map (instantiate binds . extractType . (\(_,_,x) -> x)) vbts
extractCon _ _ x = unsupported "extractCon" x

extractType :: TH.Type -> Type
extractType (AppT t1 t2) = App (extractType t1) (extractType t2)
extractType (ConT nm) = Base nm
extractType (VarT nm) = Var nm
extractType (TupleT s) = Base (TH.tupleTypeName s)
extractType ListT = Base ''[]
extractType x = unsupported "extractType" x

instantiate :: [(Name, Type)] -> Type -> Type
instantiate binds (Base name) = Base name
instantiate binds (App l r) = App (instantiate binds l) (instantiate binds r)
instantiate binds (Var v) = maybe (Var v) id (lookup v binds)



-- Traverse a data type definition, extracting recursively every data type
-- reachable from the root data type. This function also calculates mutually
-- recursive dependencies in type constructors and updates its `rec` field
-- accordingly.
reifyInvolvedTypes :: TypeDef -> Q TypeEnv
reifyInvolvedTypes root = addMutRecLoops <$> reifyInvolvedTypes' [root] root
  where
    reifyInvolvedTypes' _ this | prim this = return [this]
    reifyInvolvedTypes' visited this = do
        let newTypes = involvedWith this \\ map tsig visited
        newTypeDefs <- mapM reifyType newTypes
        newReached <- mapM (reifyInvolvedTypes' (this:visited)) newTypeDefs
        return (nub (this : concat newReached))


-- Calculate if any type constructor is mutually recursive and update the
-- `rec` field accordingly.
addMutRecLoops :: TypeEnv -> TypeEnv
addMutRecLoops env = map (addMutRecLoop env) env
  where
    addMutRecLoop env this
      = this { tcons = map (setIsRecursive env (tsig this)) (tcons this) }

    setIsRecursive env this con
        | rec con = con
        | reachableFrom env this con = con { rec = True }
        | otherwise = con

    reachableFrom env this con = any (reachableFrom' env this []) (cargs con)

    reachableFrom' env this visited arg
        | this `subtype` arg  = True
        | any (this `subtype`) argImmDefs = True
        | otherwise = any (reachableFrom' env this (arg:visited)) nextArgs
          where
            argDef = find ((==arg) . tsig) env
            argImmDefs = maybe [] involvedWith argDef
            nextArgs = maybe [] (\def -> involvedWith def \\ visited) argDef
            -- ToDo: look why some definitions are not in the env sometimes.
            -- Just nextArgs = involvedWith argDef \\ visited
