data Price = Price Integer
           deriving (Show, Eq)

data Size = Size Integer
           deriving (Show, Eq)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Show, Eq)

data Airline = PapuAir
             | CatapultsRUS
             | TakeYourChancesUnited
             deriving (Show, Eq)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Show, Eq)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 4)

vehicles :: [Vehicle]
vehicles = [myCar, urCar, clownCar, doge]

isCar :: Vehicle -> Bool
isCar (Car _ _)   = True
isCar (Plane _ _) = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu  -- this is only a partial function :/
