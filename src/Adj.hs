{-# LANGUAGE NamedFieldPuns #-}
module Adj (Adj(..), toList) where
import Data.Maybe
import Data.Foldable

data Adj a = Adj { up :: a
                 , down :: a
                 , left :: a
                 , right :: a
                 }

instance Functor Adj where
    fmap f (Adj a b c d) =
        Adj (f a) (f b) (f c) (f d)

instance Foldable Adj where
    foldMap f (Adj a b c d) =
        f a `mappend` f b `mappend` f c `mappend` f d `mappend` mempty

-- getAdj :: (Int, Int) -> Adj (Int, Int)
-- getAdj (r, c) = Adj