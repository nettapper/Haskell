data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- Generate a list of all programmers from allLanguages & allOperatingSystems
allProgrammers :: [Programmer]
allProgrammers = gen allOperatingSystems allLanguages []
  where
    gen [] _ l                 = l
    gen (os:oss) langs l       = allPairs os langs [] ++ gen oss langs l
    allPairs _ [] l            = l
    allPairs os (lang:langs) l = p os lang : allPairs os langs l
    p o l                      = Programmer { os = o, lang = l }
