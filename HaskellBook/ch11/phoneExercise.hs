import Data.Char (isUpper, toLower)

data DaPhone =
    DaPhone Key Key Key
            Key Key Key
            Key Key Key
            Key Key Key

-- 1()     2(ABC) 3(DEF)
-- 4(GHI)  5(JKL) 6(MNO)
-- 7(PQRS) 8(TUV) 9(WXYZ)
-- *(^)    0(+_)  #(.,)
aPhone :: DaPhone
aPhone = DaPhone
         (OneKey '1' '1')                  (FourKey '2' 'a' 'b' 'c' '2') (FourKey '3' 'd' 'e' 'f' '3')
         (FourKey '4' 'g' 'h' 'i' '4')     (FourKey '5' 'j' 'k' 'l' '5') (FourKey '6' 'm' 'n' 'o' '7')
         (FiveKey '7' 'p' 'q' 'r' 's' '7') (FourKey '8' 't' 'u' 'v' '8') (FiveKey '9' 'w' 'x' 'y' 'z' '9')
         (OneKey '*' '^')                  (TwoKey '0' ' ' '0')          (TwoKey '#' '.' ',')

data Key =
    OneKey   Digit Char
  | TwoKey   Digit Char Char
  | ThreeKey Digit Char Char Char
  | FourKey  Digit Char Char Char Char
  | FiveKey  Digit Char Char Char Char Char

convo :: [String]
convo =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c = if isUpper c
                         then reverseTapsLower phone '^' ++ reverseTapsLower phone (toLower c) -- TODO: search for '^' key
                         else reverseTapsLower phone c

keysList :: DaPhone -> [Key]
keysList (DaPhone a b c
                  d e f
                  g h i
                  j k l) = [a, b, c, d, e, f, g, h, i, j, k, l]

reverseTapsLower :: DaPhone -> Char -> [(Digit, Presses)]
reverseTapsLower phone = reverseTapsLowerOnKeys (keysList phone)

reverseTapsLowerOnKeys :: [Key] -> Char -> [(Digit, Presses)]
reverseTapsLowerOnKeys ks c = foldl getDigitPresses [] ks
  where getDigitPresses xs k = if snd (numOfTaps k c) >= 0
                                  then numOfTaps k c : xs
                                  else [] ++ xs

numOfTaps :: Key -> Char -> (Digit, Presses)
numOfTaps (OneKey key c1) targetChar              = (key, indexOf targetChar [c1])
numOfTaps (TwoKey key c1 c2) targetChar           = (key, indexOf targetChar [c1, c2])
numOfTaps (ThreeKey key c1 c2 c3) targetChar      = (key, indexOf targetChar [c1, c2, c3])
numOfTaps (FourKey key c1 c2 c3 c4) targetChar    = (key, indexOf targetChar [c1, c2, c3, c4])
numOfTaps (FiveKey key c1 c2 c3 c4 c5) targetChar = (key, indexOf targetChar [c1, c2, c3, c4, c5])

indexOf :: Char -> String -> Presses
indexOf targetChar charsToCheck = indexOfHelper charsToCheck targetChar (0 :: Presses)
  where indexOfHelper [] _ _ = -1 :: Presses  --  would returning 'Maybe a' would be better?
        indexOfHelper (x:xs) target count = if target == x
                                               then count + 1
                                               else indexOfHelper xs target (count + 1)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead daPhone = concatMap (reverseTaps daPhone)
