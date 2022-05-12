import Data.Char
import Text.Read
import Control.Monad

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

readInts :: Int -> IO [Int]
readInts 0 = return []
readInts n = do
  x <- readLn
  y <- readInts (n-1)
  return (x : y)

calcRead :: Int -> Int -> IO Int
calcRead op n =
  if op == 1 then calcRead' op n 0 else calcRead' op n 1
    where
      calc 1 acc x = acc + x
      calc 2 acc x = acc * x

      calcRead' op 0 acc = return acc
      calcRead' op n acc = do
        x <- readLn
        calcRead' op (n-1) (calc op acc x)

-- Now we keep asking the user infinitely for two numbers to add!
inf_input = do
  putStrLn "Which operation would you like to do?"
  operation <- readOperation

  putStr "How many numbers would you like to (sequentially) apply the selected operation to? "
  buf <- getLine
  let n = (read buf :: Int)

  putStrLn "Enter the numbers line by line:"
  test <- readInts n

  print test
  
  -- rta <- calcRead operation n
  -- putStrLn $ "The result is " ++ (show rta) ++ "!\n";
 
  inf_input

main = inf_input
