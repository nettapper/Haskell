-- This file is used to count the workout totals. 
-- This challenge is inspired from the book "Unbreakable — Thom Shea".
--
-- To follow this workout you'll need discipline. You should also be paying
-- attention to your 'inner dialog' aka 'your self talk'!
--
-- If you want to learn more you can read the blog post below.
-- https://connerdunn.com/2018/03/automating-the-discipline-challenge-pt1-the-code/

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


-- Starting reps for buildWorkout
pushups :: Rep
pushups = 5

situps :: Rep
situps = 2*pushups

squats :: Rep
squats = 3*pushups

-- An infinite list of nums used to build days
nums :: [Day]
nums = [0..]

-- An infinite list of days used to build workouts
-- two per day => [0,0,1,1,2,2 ...]
days :: [Day]
days = concatMap f z  -- apply f to every element of z, and then concat the results together
  where f (a,b) = [a,b]
        z = zip nums nums  -- z = [(0,0), (1,1), ...]

-- Builds a workout based on the current day
-- This is required because every week the workouts double
buildWorkout :: Day -> Workout
buildWorkout day = Workout {
    pushCount = weekTarget * pushups
  , sitCount = weekTarget * situps
  , squatCount = weekTarget * squats
  }
  where week = (day `div` 7)  -- week = 0 should yeild a modifer of 2^0 = 1
        weekTarget = 2^week   -- week = 1 should double the workouts, and so on

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
        finiteWorkouts = take (2 * len) workouts
        w = total finiteWorkouts
    putStrLn $ "The workout totals for " ++ show len ++ " days are: " ++ show w

