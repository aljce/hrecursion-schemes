{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.HFunctor.Foldable where

import Data.Kind (type Type)
import Data.Function (on)

import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Const (Const(..))

type f ~> g = forall a. f a -> g a

class HFunctor t where
  hfmap :: (f ~> g) -> t f ~> t g

type family Base (t :: k -> Type) :: (k -> Type) -> k -> Type

class HFunctor (Base t) => Recursive t where
  project :: t ~> Base t t
  cata :: forall f. (Base t f ~> f) -> t ~> f
  cata f = c
    where c :: t ~> f
          c = f . hfmap c . project
  para :: forall f. (Base t (Product t f) ~> f) -> t ~> f
  para f = c
    where c :: t ~> f
          c = f . hfmap (\t -> Pair t (c t)) . project

newtype HFix (f :: (k -> Type) -> k -> Type) (a :: k) = HFix { unHFix :: f (HFix f) a }

instance Show (f (HFix f) a) => Show (HFix f a) where
  showsPrec p (HFix f) = showsPrec p f

instance Eq (f (HFix f) a) => Eq (HFix f a) where
  (==) = (==) `on` unHFix

instance Ord (f (HFix f) a) => Ord (HFix f a) where
  compare = compare `on` unHFix

type instance Base (HFix f) = f

instance HFunctor f => Recursive (HFix f) where
  project = unHFix

data ExprF :: (Type -> Type) -> Type -> Type where
  EInt   :: Int -> ExprF f Int
  EBool  :: Bool -> ExprF f Bool
  EAdd   :: f Int -> f Int -> ExprF f Int
  EEqual :: f Int -> f Int -> ExprF f Bool
  EIf    :: f Bool -> f a -> f a -> ExprF f a

type Expr = HFix ExprF

instance HFunctor ExprF where
  hfmap _ (EInt i) = EInt i
  hfmap _ (EBool b) = EBool b
  hfmap eta (EAdd e1 e2) = EAdd (eta e1) (eta e2)
  hfmap eta (EEqual e1 e2) = EEqual (eta e1) (eta e2)
  hfmap eta (EIf b e1 e2) = EIf (eta b) (eta e1) (eta e2)

int :: Int -> Expr Int
int = HFix . EInt

bool :: Bool -> Expr Bool
bool = HFix . EBool

add :: Expr Int -> Expr Int -> Expr Int
add e1 e2 = HFix (EAdd e1 e2)

equal :: Expr Int -> Expr Int -> Expr Bool
equal e1 e2 = HFix (EEqual e1 e2)

iff :: Expr Bool -> Expr a -> Expr a -> Expr a
iff b e1 e2 = HFix (EIf b e1 e2)

test :: Expr Int
test = iff (int 1 `equal` int 1) (int 5 `add` int 6) (int 7)

eval :: Expr a -> a
eval = runIdentity . cata alg
  where alg :: ExprF Identity ~> Identity
        alg (EInt i) = pure i
        alg (EBool b) = pure b
        alg (EAdd e1 e2) = liftA2 (+) e1 e2
        alg (EEqual e1 e2) = liftA2 (==) e1 e2
        alg (EIf b e1 e2) = b >>= \case
          True  -> e1
          False -> e2

pprint :: Expr a -> String
pprint = getConst . cata alg
  where alg :: ExprF (Const String) ~> Const String
        alg = \case
          EInt i  -> Const (show i)
          EBool b -> Const (show b)
          EAdd e1 e2 -> Const (getConst e1 ++ " + " ++ getConst e2)
          EEqual e1 e2 -> Const (getConst e1 ++ " == " ++ getConst e2)
          EIf b e1 e2 -> Const ("if " ++ getConst b ++ " then " ++ getConst e1 ++ " else " ++ getConst e2)
