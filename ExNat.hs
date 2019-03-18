module ExNat where

import Prelude
	( Show(..)
	, Eq(..)
	, Ord(..)
	, Num(..)
	, Integral
	, Bool(..)
	, not
	, (&&)
	, (!!)
	, (++)
	, ($)
	, (.)
	, undefined
	, error
	, otherwise
	)

data Nat = Zero | Succ Nat

instance Show Nat where

	show Zero	= "0"
	show (Succ n)	= "S" ++ show n

instance Eq Nat where

	Zero	== Zero 	= True
	Succ _	== Zero 	= False
	zero	== Succ _	= False
	Succ m	== Succ n	= m == n

instance Ord Nat where

	Zero 	<= _		= True
	Succ _	<= Zero		= False
	Succ m	<= Succ n	= m <= n

	min Zero _		= Zero
	min _ Zero		= Zero
	min (Succ m) (Succ n)	= Succ $ min m n

	max Zero n		= n
	max m Zero		= m
	max (Succ m) (Succ n)	= Succ $ max m n

isZero :: Nat -> Bool
isZero Zero	= True
isZero _	= False

pred :: Nat -> Nat
pred Zero	= Zero
pred (Succ m)	= m

even :: Nat -> Bool
even Zero	= True
even (Succ m)	= odd m

odd :: Nat-> Bool
odd Zero	= False
odd (Succ m)	= even m

(<+>) :: Nat -> Nat -> Nat
Zero	<+> n		= n
m	<+> Zero	= m
(Succ m)<+> (Succ n)	= Succ (n <+> (Succ m))

(<*>) :: Nat -> Nat -> Nat
_	<*> Zero	= Zero
m	<*> Succ n	= m <+> (m <*> n)

(<^>) :: Nat -> Nat -> Nat
_	<^> Zero 	= Succ Zero
m	<^> (Succ n)	= m <*> (m <^> n)
 	
