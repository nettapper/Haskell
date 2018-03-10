-- TODO explain this file
--

-- imports
import Control.Monad(when)


-- add my types
data Workout = Workout {
    pushCount :: Rep
  , sitCount :: Rep
  , squatCount :: Rep
  } deriving (Show)

instance Monoid Workout where
  mempty = Workout {
      pushCount = 0
    , sitCount = 0
    , squatCount = 0
    }
  mappend w1 w2 = Workout {
      pushCount = pushCount w1 + pushCount w2
    , sitCount = sitCount w1 + sitCount w2
    , squatCount = squatCount w1 + squatCount w2
    }

type Day = Integer
type Rep = Integer


-- Starting numbers for buildWorkout
pushups :: Rep
pushups = 5

situps :: Rep
situps = 2*pushups

squats :: Rep
squats = 3*pushups

-- An infinite list of days used to build workouts
days :: [Day]
days = [0..]

-- Builds a workout based on the current day
-- This is required becase every week the workouts double
buildWorkout :: Day -> Workout
buildWorkout day = Workout {
    pushCount = weekTarget * pushups
  , sitCount = weekTarget * situps
  , squatCount = weekTarget * squats
  }
  where week = (day `div` 7)
        weekTarget = 2^week

-- An infinite list of workouts
workouts :: [Workout]
workouts = map buildWorkout days

-- This will reduce a finite list of workouts to a singe workout summation
total :: [Workout] -> Workout
total = mconcat

main :: IO ()
main = do
  putStrLn "This program will calculate you workout totals."
  putStr "Please enter the number of day(s):"
  lenStr <- getLine
  let r = reads lenStr
  when (not $ null r) $ do
    let len = fst $ head r
        finiteWorkouts = take len workouts
        w = total finiteWorkouts
    putStrLn $ "The workout totals for " ++ show len ++ " days are: " ++ show w

