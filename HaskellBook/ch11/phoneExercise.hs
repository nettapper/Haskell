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
         (OneKey '1')                  (FourKey 'a' 'b' 'c' '2') (FourKey 'd' 'e' 'f' '3')
         (FourKey 'g' 'h' 'i' '4')     (FourKey 'j' 'k' 'l' '5') (FourKey 'm' 'n' 'o' '6')
         (FiveKey 'p' 'q' 'r' 's' '7') (FourKey 't' 'u' 'v' '8') (FiveKey 'w' 'x' 'y' 'z' '9')
         (OneKey '^')                  (TwoKey ' ' '0')          (TwoKey '.' ',')

data Key =
    OneKey   Char
  | TwoKey   Char Char
  | ThreeKey Char Char Char
  | FourKey  Char Char Char Char
  | FiveKey  Char Char Char Char Char

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
                         then ('^', 1) : reverseTapsLower phone (toLower c) -- TODO: search for '^' key
                         else reverseTapsLower phone c

keysList :: DaPhone -> [Key]
keysList = undefined

reverseTapsLower :: DaPhone -> Char -> [(Digit, Presses)]
reverseTapsLower phone = reverseTapsLowerOnKeys (keysList phone)

reverseTapsLowerOnKeys :: [Key] -> Char -> [(Digit, Presses)]
reverseTapsLowerOnKeys = undefined

numOfTaps :: Key -> Char -> (Digit, Presses)
numOfTaps (OneKey c1) targetChar              = indexOf targetChar [c1]
numOfTaps (TwoKey c1 c2) targetChar           = indexOf targetChar [c1, c2]
numOfTaps (ThreeKey c1 c2 c3) targetChar      = indexOf targetChar [c1, c2, c3]
numOfTaps (FourKey c1 c2 c3 c4) targetChar    = indexOf targetChar [c1, c2, c3, c4]
numOfTaps (FiveKey c1 c2 c3 c4 c5) targetChar = indexOf targetChar [c1, c2, c3, c4, c5]

indexOf :: Char -> String -> (Digit, Presses)
indexOf targetChar charsToCheck = indexOfHelper charsToCheck targetChar (0 :: Int)
  where indexOfHelper [] _ _ = (' ', -1)  --  would returning 'Maybe a' would be better?
        indexOfHelper (x:xs) target count = if target == x
                                               then (target, count + 1)
                                               else indexOfHelper xs target (count + 1)

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead = undefined
