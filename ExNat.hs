module ExNat where

import Prelude ( Show(..), Eq(..), Bool(..), (++) )

data Nat = Zero | Succ Nat

instance Show Nat where

 show Zero     = "0"
 show (Succ m) = "S" ++ show m

instance Eq Nat where

 Zero     == Zero     = True
 Zero     == (Succ _) = False
 (Succ _) == Zero     = False
 (Succ m) == (Succ n) = m == n

