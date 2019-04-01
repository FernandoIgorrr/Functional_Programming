import System.IO
import Text.Read
import Control.Monad
import ExList

main :: IO ()
main = do
 putStrLn "Digite um número para o tamanho da sua lista:"
 inputInt <- getLine
 let n = (read inputInt :: Int)
 inputs   <- getUserInputs n
 putStrLn "---------------------------------------------"
 print inputs 
 putStrLn "---------------------------------------------"
 putStrLn "Agora ordenado do menor para o maior:"
 putStrLn "---------------------------------------------"
 print $ myQSort inputs 
 putStrLn "---------------------------------------------"
 putStrLn "Agora ordenado do maior para o menor:"
 putStrLn "---------------------------------------------"
 print $ myReverse $ myQSort inputs 
 putStrLn "---------------------------------------------"
parseInput :: String -> Int -> Maybe Int
parseInput input i
 |i <= 0    = Nothing
 |otherwise = (readMaybe input :: Maybe Int)

getUserInputs :: Int -> IO [Int]
getUserInputs n
 |n <= 0    = return []
 |otherwise = do
  hFlush stdout
  putStrLn "Digite um número:"
  input <- getLine
  case parseInput input n of
   Nothing     -> return []
   Just anInt -> do
   moreInputs <- getUserInputs $ n - 1
   return $ anInt : moreInputs
 
