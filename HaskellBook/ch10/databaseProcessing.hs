import Data.Time

data DatabaseItem =
    DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr onlyUTCTime []
  where onlyUTCTime :: DatabaseItem -> [UTCTime] -> [UTCTime]
        onlyUTCTime dbItem utcList = case dbItem of
                                          (DbString _) -> utcList
                                          (DbNumber _) -> utcList
                                          (DbDate utc) -> utc : utcList

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr onlyDbNumber []
  where onlyDbNumber :: DatabaseItem -> [Integer] -> [Integer]
        onlyDbNumber dbItem integerList = case dbItem of
                                               (DbString _) -> integerList
                                               (DbNumber i) -> i : integerList
                                               (DbDate _) -> integerList

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = undefined

sumDb :: [DatabaseItem] -> Integer
sumDb = undefined

-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb = undefined
