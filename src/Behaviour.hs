module Behaviour(Animation, Combine, getFrame, time, 
  translateTime, lift0, lift1, lift2) where

import Data.Functor


newtype Animation a = Animation{animation :: Time -> a}
type Time = Float

getFrame :: Animation a -> Time -> a
getFrame (Animation f) t = f(t)

time :: Animation Time
time = Animation(\t -> t)

translateTime :: Time -> Animation Time -> Animation Time
translateTime time animation = lift0(time) + animation

lift0 :: a -> Animation a
lift0 a = Animation(\_ -> a)

lift1 :: (a -> b) -> (Animation a -> Animation b)
lift1 f animation = Animation(f.getFrame(animation))

lift2 :: (a -> b -> c) -> (Animation a -> Animation b -> Animation c)
lift2 f animation1 animation2 = 
  Animation(\t -> f(getFrame(animation1)(t))(getFrame(animation2)(t)))

instance Num a => Num(Animation a) where
  (+) = lift2(+)
  (*) = lift2(*)
  negate = lift1 negate
  abs = lift1 abs
  signum = lift1 signum
  fromInteger = lift0.fromInteger

class Combine a where
  combine :: a
  over :: a -> a -> a


instance Combine a => Combine(Animation a) where
  combine = lift0(combine)
  over = lift2(over)

timeTrans :: Animation Time -> Animation a -> Animation a
timeTrans btime ba = Animation (animation(ba).animation(btime))

instance Functor(Animation) where
  fmap = lift1 

