module Data.Time.Calendar.DateConversionsSpec
    ( main
    , spec
    ) where

import qualified Data.Dates as D
import qualified Data.Time as T
import Data.Time.Calendar.DateConversions hiding (Day)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary T.Day where
    arbitrary = T.fromGregorian <$> choose (1600, 2400) <*> choose (1, 12) <*> choose (1, 31)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    context "quickcheck" $
    parallel $
    modifyMaxSuccess (* 1000) $ do
        describe "beginningOfWeek" $
            it "returns Sunday" $
            property $ \d -> dayToWeekDay (beginningOfWeek d) `shouldBe` D.Sunday
        describe "beginningOfBiweek" $
            it "returns a Sunday in a two-week window" $
            property $ \d -> do
                dayToWeekDay (beginningOfBiweek d) `shouldBe` D.Sunday
                T.diffDays (beginningOfBiweek d) d `shouldSatisfy` (<= 13)
                T.diffDays (beginningOfBiweek d) d `shouldSatisfy` (>= -13)
        describe "beginningOfMonth" $
            it "retains the month independent of day" $
            property $ \d -> do
                dayFromDay (beginningOfMonth d) `shouldBe` 1
                monthFromDay (beginningOfMonth d) `shouldBe` monthFromDay d
                yearFromDay (beginningOfMonth d) `shouldBe` yearFromDay d
        describe "beginningOfQuarter" $
            it "calculates the correct month and day independent of start" $
            property $ \d -> do
                [1, 4, 7, 10] `shouldContain` [monthFromDay (beginningOfQuarter d)]
                dayFromDay (beginningOfQuarter d) `shouldBe` 1
                yearFromDay (beginningOfQuarter d) `shouldBe` yearFromDay d
        describe "beginningOfYear" $
            it "calculates the correct month and day independent of start" $
            property $ \d -> do
                monthFromDay (beginningOfYear d) `shouldBe` 1
                dayFromDay (beginningOfYear d) `shouldBe` 1
                yearFromDay (beginningOfYear d) `shouldBe` yearFromDay d
        describe "endOfWeek" $
            it "returns the correct Saturday" $
            property $ \d -> do
                dayToWeekDay (endOfWeek d) `shouldBe` D.Saturday
                endOfWeek d `shouldSatisfy` (>= d)
                T.diffDays (endOfWeek d) d `shouldSatisfy` (<= 7)
        describe "endOfBiweek" $
            it "returns the correct Saturday" $
            property $ \d -> do
                dayToWeekDay (endOfBiweek d) `shouldBe` D.Saturday
                endOfBiweek d `shouldSatisfy` (>= d)
                T.diffDays (endOfBiweek d) d `shouldSatisfy` (<= 13)
        describe "endOfMonth" $
            it "retains the month and year independent of day" $
            property $ \d -> do
                monthFromDay (endOfMonth d) `shouldBe` monthFromDay d
                yearFromDay (endOfMonth d) `shouldBe` yearFromDay d
        describe "endOfQuarter" $
            it "retains the month and year independent of day" $
            property $ \d -> do
                [12, 3, 6, 9] `shouldContain` [monthFromDay (endOfQuarter d)]
                yearFromDay (endOfQuarter d) `shouldBe` yearFromDay d
                endOfQuarter d `shouldSatisfy` (>= d)
                T.diffDays (endOfQuarter d) d `shouldSatisfy` (<= 100)
        describe "endOfYear" $
            it "retains the month and year independent of day" $
            property $ \d -> do
                yearFromDay (endOfYear d) `shouldBe` yearFromDay d
                monthFromDay (endOfYear d) `shouldBe` 12
                dayFromDay (endOfYear d) `shouldBe` 31
                T.diffDays (endOfYear d) d `shouldSatisfy` (<= 365)
        describe "nextWeek" $
            it "returns a Sunday within the next seven days" $
            property $ \d -> do
                dayToWeekDay (nextWeek d) `shouldBe` D.Sunday
                nextWeek d `shouldBe` T.addDays 7 (beginningOfWeek d)
                T.diffDays (nextWeek d) d `shouldSatisfy` (<= 7)
                T.diffDays (nextWeek d) d `shouldSatisfy` (> 0)
        describe "nextBiweek" $
            it "returns a Sunday within the next 14 days" $
            property $ \d -> do
                dayToWeekDay (nextBiweek d) `shouldBe` D.Sunday
                nextBiweek d `shouldBe` T.addDays 14 (beginningOfBiweek d)
                T.diffDays (nextBiweek d) d `shouldSatisfy` (<= 14)
                T.diffDays (nextBiweek d) d `shouldSatisfy` (> 0)
        describe "nextMonth" $
            it "returns the first day of the next month" $
            property $ \d -> do
                dayFromDay (nextMonth d) `shouldBe` 1
                monthFromDay (nextMonth d) `shouldBe`
                    if monthFromDay d == 12
                        then 1
                        else monthFromDay d + 1
                yearFromDay (nextMonth d) `shouldBe`
                    if monthFromDay d == 12
                        then yearFromDay d + 1
                        else yearFromDay d
        describe "nextQuarter" $
            it "returns the first day of the next quarter" $
            property $ \d -> do
                [1, 4, 7, 10] `shouldContain` [monthFromDay (nextQuarter d)]
                dayFromDay (nextQuarter d) `shouldBe` 1
                ((monthFromDay (beginningOfQuarter d) + 3) `mod` 12) `shouldBe`
                    monthFromDay (nextQuarter d)
                T.diffDays (nextQuarter d) d `shouldSatisfy` (<= 100)
                T.diffDays (nextQuarter d) d `shouldSatisfy` (> 0)
        describe "nextYear" $
            it "returns January 1 of the next year" $
            property $ \d -> do
                monthFromDay (nextYear d) `shouldBe` 1
                dayFromDay (nextYear d) `shouldBe` 1
                yearFromDay (nextYear d) `shouldBe` yearFromDay d + 1
        describe "previousWeek" $
            it "returns a Sunday within the past seven days" $
            property $ \d -> do
                dayToWeekDay (previousWeek d) `shouldBe` D.Sunday
                previousWeek d `shouldBe` T.addDays (-7) (beginningOfWeek d)
                T.diffDays (previousWeek d) d `shouldSatisfy` (<= -7)
                T.diffDays (previousWeek d) d `shouldSatisfy` (> -14)
        describe "previousBiweek" $
            it "returns a Sunday within the past fourteen days" $
            property $ \d -> do
                dayToWeekDay (previousBiweek d) `shouldBe` D.Sunday
                previousBiweek d `shouldBe` T.addDays (-14) (beginningOfBiweek d)
                T.diffDays (previousBiweek d) d `shouldSatisfy` (<= -14)
                T.diffDays (previousBiweek d) d `shouldSatisfy` (> -28)
        describe "previousMonth" $
            it "returns the first day of the previous month" $
            property $ \d -> do
                dayFromDay (previousMonth d) `shouldBe` 1
                monthFromDay (previousMonth d) `shouldBe`
                    if monthFromDay d == 1
                        then 12
                        else monthFromDay d - 1
                yearFromDay (previousMonth d) `shouldBe`
                    if monthFromDay d == 1
                        then yearFromDay d - 1
                        else yearFromDay d
        describe "previousQuarter" $
            it "returns the first day of the previous quarter" $
            property $ \d -> do
                [1, 4, 7, 10] `shouldContain` [monthFromDay (previousQuarter d)]
                dayFromDay (previousQuarter d) `shouldBe` 1
                ((monthFromDay (beginningOfQuarter d) + 9) `mod` 12) `shouldBe`
                    monthFromDay (previousQuarter d)
                T.diffDays d (previousQuarter d) `shouldSatisfy` (<= 183)
                T.diffDays d (previousQuarter d) `shouldSatisfy` (>= 90)
        describe "previousYear" $
            it "returns January 1 of the previous year" $
            property $ \d -> do
                monthFromDay (previousYear d) `shouldBe` 1
                dayFromDay (previousYear d) `shouldBe` 1
                yearFromDay (previousYear d) `shouldBe` yearFromDay d - 1

dayToWeekDay :: T.Day -> D.WeekDay
dayToWeekDay = D.dateWeekDay . D.dayToDateTime

dayFromDay :: T.Day -> Int
dayFromDay = fromIntegral . fromDay Day

monthFromDay :: T.Day -> Int
monthFromDay = fromIntegral . fromDay Month

yearFromDay :: T.Day -> Integer
yearFromDay = fromIntegral . fromDay Year

data Period
    = Year
    | Month
    | Day

fromDay :: Period -> T.Day -> Integer
fromDay p d =
    case p of
        Year -> toInteger year
        Month -> toInteger month
        Day -> toInteger day
  where
    (year, month, day) = T.toGregorian d
