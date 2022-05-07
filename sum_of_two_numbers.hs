main = do
  putStr "Enter the first number: "
  a <- readLn
  putStr "Enter the second number: "
  b <- readLn
  putStrLn $ "The result is " ++ (show (a + b)) ++ "!"
