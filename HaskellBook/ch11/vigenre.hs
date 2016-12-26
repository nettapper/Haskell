keyword :: String
keyword = "ally"

message :: String
message = "meet at dawn"

encode :: String -> String -> String
encode m k = undefined

decode :: String -> String -> String
decode m k = undefined

test :: IO ()
test = if decode (encode message keyword) keyword == message
       then putStrLn "it is working"
       else putStrLn "messages don't match"

main :: IO ()
main = test
