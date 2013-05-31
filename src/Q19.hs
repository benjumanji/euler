module Q19
  where

import Data.List

data Days =
      Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Show, Eq)

days = cycle [Monday,Tuesday,Wednesday,Thursday, Friday, Saturday, Sunday]

data Months =
      January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Show)

months = cycle [January, February, March, April, May, June, July, August, September, October, November, December]

yearsIndex :: [Int]
yearsIndex = unfoldr f (1900, 0)
  where
    f (i,11) = Just (i, (i+1,0))
    f (i,x)  = Just (i, (i, x+1))

m28 = [1..28]
m29 = [1..29]
m30 = [1..30]
m31 = [1..31]

monthToDays  :: (Months, Int) -> [Int]
monthToDays (February, x) | x `rem` 400 == 0 = m29
                          | otherwise = m28
monthToDays (April, _) = m30
monthToDays (June, _) = m30
monthToDays (September, _) = m30
monthToDays (November, _) = m30
monthToDays (_, _) = m31

years = dropWhile (<=1900) $ takeWhile (<=2000) yearsIndex

monthYear = zipWith (,) months years
dayOfMonthIndex = monthYear >>= monthToDays
dayDayOfMonth = zipWith (,) days dayOfMonthIndex

answer = sum . map snd .filter (\(day,dom) -> day == Sunday && dom == 1) $ dayDayOfMonth





