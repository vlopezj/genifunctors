{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
#endif
module TestTypes where

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

data family Sing (a :: k)
data instance Sing (a :: Bool) where
    SFalse :: Sing False
    STrue  :: Sing True

#if __GLASGOW_HASKELL__ >= 708
data ZB a b (τ :: Bool) where
  ZT :: a -> ZB a b True
  ZF :: b -> ZB a b False

data Z a (τ :: Bool) b where
  A1 :: a       -> Z a False b
  A2 :: (a,a)   -> Z a True  b 
  A3 :: b       -> Z a False b
--  AB :: ZB a Int τ -> Z a τ
  AN :: Sing b -> [a] -> Z a b c
#endif
