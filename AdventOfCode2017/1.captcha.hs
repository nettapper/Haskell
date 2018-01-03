
captcha :: String -> Integer
captcha [] = 0
captcha ds@(d:_) = seqSum $ ds ++ [d]

seqSum :: String -> Integer
seqSum [] = 0
seqSum [a] = 0
seqSum (a:b:others) = if a == b
                         then (read (b:"") :: Integer) + seqSum (b:others)
                         else seqSum (b:others)

main :: IO ()
main = do
  putStrLn $ show $ captcha "1122"
  putStrLn $ show $ captcha "1111"
  putStrLn $ show $ captcha "1234"
  putStrLn $ show $ captcha "91212129"
