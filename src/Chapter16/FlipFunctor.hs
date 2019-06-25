{-# LANGUAGE FlexibleInstances #-}

module Chapter16.FlipFunctor where

data Tuple a b =
  Tuple a
        b
  deriving (Eq, Show)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b
