module Vec (Vec3(..)) where

import Control.Applicative

data Vec3 a = Vec3 a a a
  deriving (Show, Eq, Ord)

instance Num a => Num (Vec3 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
    pure x = Vec3 x x x
    liftA2 f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
        Vec3 (f x1 x2) (f y1 y2) (f z1 z2)

instance Foldable Vec3 where
    foldr f b (Vec3 a0 a1 a2) = f a0 (f a1 (f a2 b))
