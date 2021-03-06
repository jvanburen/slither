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

zip :: Adj a -> Adj b -> Adj (a, b)
zip (Adj a1 a2 a3 a4) (Adj b1 b2 b3 b4) =
    Adj (a1, b1) (a2, b2) (a3, b3) (a4, b4)

zip3 :: Adj a -> Adj b -> Adj c -> Adj (a, b, c)
zip3 (Adj a1 a2 a3 a4) (Adj b1 b2 b3 b4) (Adj c1 c2 c3 c4) =
    Adj (a1, b1, c1) (a2, b2, c2) (a3, b3, c3) (a4, b4, c4)

-- getAdj :: (Int, Int) -> Adj (Int, Int)
-- getAdj (r, c) = Adj