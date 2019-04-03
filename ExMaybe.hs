module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) =
 case x of
  Nothing    -> catMaybes xs
  Just thing -> thing : catMaybes xs

myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup _ []     = Nothing
myLookup x ((y,z):ys)
 |x == y    = Just z
 |otherwise = myLookup x ys 
 
fromJust :: Maybe a -> a
fromJust x =
 case x of
  Nothing -> error "Nothing"
  Just thing -> thing

fromMaybe :: a -> Maybe a -> a
fromMaybe x y =
 case y of
  Just thing -> thing
  Nothing    -> x

isJust :: Maybe a -> Bool
isJust x =
 case x of
  Just thing -> True
  otherwise  -> False

isNothing :: Maybe a -> Bool
isNothing x =
 case x of
  Nothing   -> True
  otherwise -> False

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 case f x of 
  Nothing -> mapMaybe f xs
  otherwise -> fromJust (f x) : mapMaybe f xs 

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x f m =
 case m of
  Nothing   -> x
  ohterwise -> f (fromJust m)

maybeToList :: Maybe a -> [a]
maybeToList x = 
 case x of
  Nothing -> []
  otherwise -> [fromJust x]


