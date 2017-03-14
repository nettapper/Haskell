type Luck     = Bool
type HardWork = Bool
data MaybeSucess   = Success | Failure
  deriving (Show)

life :: (Luck, HardWork) -> MaybeSucess
life (luck, work) = if signifigant
                       then Success
                       else Failure
                         where signifigant = luck && work

