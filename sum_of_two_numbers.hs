-- Now we keep asking the user infinitely for two numbers to add!
inf_input = do
  putStr "Enter the first number: "
  a <- readLn
  putStr "Enter the second number: "
  b <- readLn
  putStrLn $ "The result is " ++ (show (a + b)) ++ "!\n"
  inf_input

main = inf_input
