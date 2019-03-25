module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

myFactorial :: Integer -> Integer
myFactorial 0	= 1
myFactorial n	|n < 0		= error "Sub zero"
		|otherwise	= n * myFactorial (n - 1)

myHead :: [a] -> a
myHead []	= error "Empty List"
myHead (x:xs)	= x

myTail :: [a] -> [a]
myTail []	= error "Empty List"
myTail (_:xs)	= xs

myLast :: [a] -> a
myLast []	= error "Empty List"
myLast [x]	= x
myLast (_:xs)	= myLast xs

myInit :: [a] -> [a]
myInit []	= error "Empty List"
myInit [x]	= []
myInit (x:xs)	= x : myInit xs

myLength :: Integral i => [a] -> i
myLength []	= 0
myLength (x:xs)	= 1 + myLength xs

myNull :: [a] -> Bool
myNull []	= True
myNull _	= False

myReverse' :: [a] -> [a]
myReverse' [x]	= [x]
myReverse' (x:xs)= myReverse' xs ++ [x]

myReverse :: [a] -> [a]
myReverse xs = myReverse_ xs []

myReverse_ :: [a] -> [a] -> [a] 
myReverse_ [] ys	= ys
myReverse_ (x:xs) ys	= myReverse_ xs (x:ys) 

myTake :: Integral i => i -> [a] -> [a]
myTake _ []	= []
myTake 0 _	= [] 
myTake n (x:xs)	= x : myTake (n - 1) xs

myDrop :: Integral i => i -> [a] -> [a]
myDrop 0 xs	= xs
myDrop _ []	= []
myDrop n (x:xs)	= myDrop (n -1) xs

myMaximum :: Ord a => [a] -> a
myMaximum [] 	= error "Empty List"
myMaximum [x]	= x
myMaximum (x:xs)|x > max	= x
		|otherwise	= max
	where max = myMaximum xs 

myMinimum :: Ord a => [a] -> a
myMinimum []	= error "Empety List"
myMinimum [x]	= x
myMinimum (x:xs)|x < min	= x
		|otherwise	= min
	where min = myMinimum xs

mySum :: Num a => [a] -> a
mySum []	= error "Empty List"
mySum [x]	= x
mySum (x:xs)	= x + mySum xs

myProd :: Num a => [a] -> a
myProd []	= error "Empty List"
myProd [x]	= x
myProd (x:xs)	= x * myProd xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []	= False
myElem x (y:ys) = x == y || myElem x ys 

myCycle :: [a] -> [a]
myCycle [] = error "Empty List"
myCycle xs = xs ++ myCycle xs

myRepeat :: a -> [a]
myRepeat n = n : myRepeat n 

myFst :: (a,b) -> a
myFst (x,_)	= x

mySnd :: (a,b) -> b
mySnd (_,y)	= y

myZip :: [a] -> [b] -> [(a,b)]
myZip _		[]	= []
myZip []	_	= []
myZip (x:xs)	(y:ys)	= (x,y) : myZip xs ys

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip []		= error "Empty list"
myUnzip [(x,y)] 	= ([x],[y])
myUnzip ((x,y):xs)	= (x: myFst l, y: mySnd l)
	where
	l = myUnzip xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z []		= z
myFoldr f z (x:xs)	= f x (myFoldr f z xs) 

(++) :: [a] -> [a] -> [a]
[]	++ ys	= ys
(x:xs)	++ ys	= x : (xs ++ ys)

