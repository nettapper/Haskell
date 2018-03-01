
-- Note: right now this is only for the mornings

pushups = 5
situps = 2*pushups
squats = 3*pushups

days :: [Integer]
days = [0..]

workout :: Integer -> (Integer, Integer, Integer)
workout day = (weekTarget*pushups, weekTarget*situps, weekTarget*squats)
  where week = (day `div` 7)
        weekTarget = 2^week

done :: [(Integer, Integer, Integer)]
done = map workout days

total :: [(Integer, Integer, Integer)] -> (Integer, Integer, Integer)
total done = myTripleSum (0,0,0) done

myTripleSum init list = foldl f init list
  where f (init1, init2, init3) (list1, list2, list3) = (init1 + list1, init2 + list2, init3 + list3)
