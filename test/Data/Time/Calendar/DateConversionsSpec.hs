module Data.Time.Calendar.DateConversionsSpec
    ( main
    , spec
    ) where

import qualified Data.Dates                         as D
import qualified Data.Time                          as T
import           Data.Time.Calendar.DateConversions hiding (Day)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary T.Day where
    arbitrary =
        T.fromGregorian
        <$> choose (1600, 2400)
        <*> choose (1, 12)
        <*> choose (1, 31)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    context "quickcheck" $ parallel $ modifyMaxSuccess (* 1000) $ do
        describe "beginningOfWeek" $
            it "returns Sunday" $ property $ \d ->
                dayToWeekDay (beginningOfWeek d) `shouldBe` D.Sunday

        describe "beginningOfMonth" $
            it "retains the month independent of day" $ property $ \d -> do
                dayFromDay (beginningOfMonth d) `shouldBe` 1
                monthFromDay (beginningOfMonth d) `shouldBe` monthFromDay d
                yearFromDay (beginningOfMonth d) `shouldBe` yearFromDay d

        describe "beginningOfQuarter" $
            it "calculates the correct month and day independent of start" $ property $ \d -> do
                [1, 4, 7, 10] `shouldContain` [monthFromDay (beginningOfQuarter d)]
                dayFromDay (beginningOfQuarter d) `shouldBe` 1
                yearFromDay (beginningOfQuarter d) `shouldBe` yearFromDay d

        describe "beginningOfYear" $
            it "calculates the correct month and day independent of start" $ property $ \d -> do
                monthFromDay (beginningOfYear d) `shouldBe` 1
                dayFromDay (beginningOfYear d) `shouldBe` 1
                yearFromDay (beginningOfYear d) `shouldBe` yearFromDay d

        describe "endOfWeek" $
            it "returns the correct Saturday" $ property $ \d -> do
                dayToWeekDay (endOfWeek d) `shouldBe` D.Saturday
                endOfWeek d `shouldSatisfy` (>= d)
                T.diffDays (endOfWeek d) d `shouldSatisfy` (<= 7)

        describe "endOfMonth" $
            it "retains the month and year independent of day" $ property $ \d -> do
                monthFromDay (endOfMonth d) `shouldBe` monthFromDay d
                yearFromDay (endOfMonth d) `shouldBe` yearFromDay d

        describe "endOfQuarter" $
            it "retains the month and year independent of day" $ property $ \d -> do
                [12, 3, 6, 9] `shouldContain` [monthFromDay (endOfQuarter d)]
                yearFromDay (endOfQuarter d) `shouldBe` yearFromDay d
                endOfQuarter d `shouldSatisfy` (>= d)
                T.diffDays (endOfQuarter d) d `shouldSatisfy` (<= 100)

        describe "endOfYear" $
            it "retains the month and year independent of day" $ property $ \d -> do
                yearFromDay (endOfYear d) `shouldBe` yearFromDay d
                monthFromDay (endOfYear d) `shouldBe` 12
                dayFromDay (endOfYear d) `shouldBe` 31
                T.diffDays (endOfYear d) d `shouldSatisfy` (<= 365)

dayToWeekDay :: T.Day -> D.WeekDay
dayToWeekDay = D.dateWeekDay . D.dayToDateTime

dayFromDay :: T.Day -> Int
dayFromDay = fromIntegral . fromDay Day

monthFromDay :: T.Day -> Int
monthFromDay = fromIntegral . fromDay Month

yearFromDay :: T.Day -> Integer
yearFromDay = fromIntegral . fromDay Year

data Period = Year | Month | Day

fromDay :: Period -> T.Day -> Integer
fromDay p d = case p of
    Year -> toInteger year
    Month -> toInteger month
    Day -> toInteger day
  where
    (year, month, day) = T.toGregorian d
