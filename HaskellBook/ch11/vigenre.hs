import Data.Char

myKeyword :: String
myKeyword = "ally"

myMessage :: String
myMessage = "meet at dawn"

caesar :: Int -> Char -> Char
caesar _ ' ' = ' '
caesar numShiftRight val = denormalize (newVal val numShiftRight)
  where denormalize n = chr $ n + ord 'a'
        normalize c = ord c - ord 'a'
        newVal c shift = mod (normalize c + shift) 26

encode :: String -> String -> String
encode message keyword = map f $ myZip message (cycle keyword)
  where myZip [] _ = []
        myZip (x:xs) yss@(y:ys) = if x == ' '
                              then (x, 'a') : myZip xs yss
                              else (x, y) : myZip xs ys
        f (letter, keyletter) = caesar (numShift keyletter) letter
        numShift k = mod (ord k - ord 'a') 26

decode :: String -> String -> String
decode message keyword = undefined

test :: IO ()
test = if decode (encode myMessage myKeyword) myKeyword == myMessage
       then putStrLn "it is working"
       else putStrLn "messages don't match"

main :: IO ()
main = test
