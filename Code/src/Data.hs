module Data where

import Data.Fixed

type Name = String

-- | Value that represents a float, int or string.
data Value = Float Float
           | Int Integer
           | String String
  deriving (Eq, Ord)

-- | Extend the Show typeclass for Value.
instance Show Value where
  show (Int i) = show i
  show (Float f) = if isInt f then show (round f) else show f
  show (String s) = "\"" ++ s ++ "\""

-- | Extend the basic operations of Num for Value.
instance Num Value where
  -- addition
  (+) (Int i) (Int i') = (Int (i + i'))
  (+) (Float f) (Int i) = (Float (f + fromIntegral i))
  (+) (Int i) (Float f) = (Float (fromIntegral i + f))
  (+) (Float f) (Float f') = (Float (f + f'))
  (+) (String s) (String s') = (String (s ++ s'))
  (+) (String s) (Int i) = (String (s ++ show i))
  (+) (Int i) (String s) = (String (show i ++ s))
  (+) (String s) (Float f) = (String (s ++ show f))
  (+) (Float f) (String s) = (String (show f ++ s))
  -- subtraction
  (-) (Int i) (Int i') = (Int (i - i'))
  (-) (Float f) (Int i) = (Float (f - fromIntegral i))
  (-) (Int i) (Float f) = (Float (fromIntegral i - f))
  (-) (Float f) (Float f') = (Float (f - f'))
  -- multiplication
  (*) (Int i) (Int i') = (Int (i * i'))
  (*) (Float f) (Int i) = (Float (f * fromIntegral i))
  (*) (Int i) (Float f) = (Float (fromIntegral i * f))
  (*) (Float f) (Float f') = (Float (f * f'))
  (*) (String s) (Int i) = (String (concat $ replicate (fromIntegral i) s))
  (*) (Int i) (String s) = (String (concat $ replicate (fromIntegral i) s))
  -- absolute value
  abs (Int i) = (Int (abs i))
  abs (Float f) = (Float (abs f))

-- | Extend the basic operations of Fractional for Value.
instance Fractional Value where
  (/) (Int i) (Int i') = (Int (i `div` i'))
  (/) (Float f) (Int i) = (Float (f / fromIntegral i))
  (/) (Int i) (Float f) = (Float (fromIntegral i / f))
  (/) (Float f) (Float f') = (Float (f / f'))

-- | Extend the basic operations of Floating for Value.
instance Floating Value where
  (**) (Int i) (Int i') = (Int (i ^ i'))
  (**) (Float f) (Int i) = (Float (f ^ i))
  (**) (Int i) (Float f) = (Float ((fromIntegral i) ** f))
  (**) (Float f) (Float f') = (Float (f ** f'))

-- | Extend the basic operations of Integral for Value.
instance Integral Value where
  mod (Int i) (Int i') = (Int (mod i i'))
  mod (Float f) (Int i) = (Float (mod' f (fromIntegral i)))
  mod (Int i) (Float f) = (Float (mod' (fromIntegral i) f))
  mod (Float f) (Float f') = (Float (mod' f f'))

instance Real Value
instance Enum Value

-- |The 'isInt' function checks if the provided float is an integer.
isInt :: Float -> Bool
isInt x = x == fromInteger (round x)
