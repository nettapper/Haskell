module Print3Broken where

printSecond :: IO ()
printSecond = do
    putStrLn greeting  -- this won't work because greeting isn't in scope
    putStrLn greeting

main :: IO ()
main = do
    putStrLn greeting
    printSecond
      where greeting = "YAAARRR"
