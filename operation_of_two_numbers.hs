import Data.Char
import Text.Read
import Control.Monad

-- Reads and parses the operation selected by the user
readOperation = do
  putStr "(sum, product) [1-2]: "
  buf <- getLine
  
  let
    parseOperation xs =
      if (readMaybe xs :: Maybe Int) == Nothing
        then 
          case xs of
            "sum"           -> 1
            "product"       -> 2
            _               -> -1
        else read xs :: Int

  let operation = parseOperation (map toLower buf)

  if operation > 2 || operation < 1
  then putStrLn "\nInvalid input. Please try again." >> readOperation
  else return operation

-- Reads the numbers entered by the user into a list
readInts :: Int -> IO [Int]
readInts n | n > 0 = do
                       x <- getLine
                       let x2 = map (\x -> read x :: Int) $ words x
                       y <- readInts (n - length x2)
                       return $ take (min (length x2) n) x2 ++ y
           | True  = return [] 

-- Now we keep asking the user infinitely for two numbers to add!
inf_input = do
  putStrLn "Which operation would you like to do?"
  operation <- readOperation

  putStr "How many numbers would you like to (sequentially) apply the selected operation to? "
  buf <- getLine
  let n = (read buf :: Int)

  putStrLn "Enter the numbers separated by spaces or newlines:"
  input <- readInts n

  let result = case operation of
                 1 -> sum input
                 2 -> product input

  putStrLn $ "The result is " ++ (show result) ++ "!\n"
  
  inf_input

main = inf_input
