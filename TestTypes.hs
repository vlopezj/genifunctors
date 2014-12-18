{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
#endif
module TestTypes where

#if __GLASGOW_HASKELL__ >= 706
import GHC.TypeLits
#endif

data U a b c d
    = L [U a b c d]               -- polymorphic recursion
    | M (V (a,b) (Either c d))    -- mutually recursive
    | a :+: Int                   -- infix syntax, record syntax, type synonyms
    | R { c :: c, d :: String }   -- and primitive data types supported
 deriving (Eq,Show)

data V u v = X (U v v u u) | Z u
 deriving (Eq,Show)

data W (a :: *) b = W b
 deriving (Eq,Show)

#if __GLASGOW_HASKELL__ >= 706
data Z a (b :: Nat) where
  A1 :: a       -> Z a 1
  A2 :: (a,a)   -> Z a 2 
  A3 :: (a,a,a) -> Z a 3 
#endif
