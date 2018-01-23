module Data.Time.Calendar.DateConversions
    -- beginning
    ( beginningOfWeek
    , beginningOfMonth
    , beginningOfQuarter
    , beginningOfYear
    -- end
    , endOfWeek
    , endOfMonth
    , endOfQuarter
    , endOfYear
    -- next
    , nextWeek
    , nextMonth
    , nextQuarter
    , nextYear
    -- previous
    , previousWeek
    , previousMonth
    , previousQuarter
    , previousYear
    ) where

import qualified Data.Dates as D
import qualified Data.Time as T

beginningOfWeek :: T.Day -> T.Day
beginningOfWeek d =
    if T.addDays 1 d == nextMonday d
        then d
        else T.addDays (-8) $ nextMonday d
  where
    nextMonday = D.dateTimeToDay . D.nextMonday . D.dayToDateTime

beginningOfMonth :: T.Day -> T.Day
beginningOfMonth d = T.fromGregorian year month 1
  where
    (year, month, _) = T.toGregorian d

beginningOfQuarter :: T.Day -> T.Day
beginningOfQuarter d = T.fromGregorian year (quarter * 3 + 1) 1
  where
    (year, month, _) = T.toGregorian d
    quarter = div (month - 1) 3

beginningOfYear :: T.Day -> T.Day
beginningOfYear d = T.fromGregorian year 1 1
  where
    (year, _, _) = T.toGregorian d

endOfWeek :: T.Day -> T.Day
endOfWeek = T.addDays 6 . beginningOfWeek

endOfMonth :: T.Day -> T.Day
endOfMonth = T.addDays (-1) . T.addGregorianMonthsClip 1 . beginningOfMonth

endOfQuarter :: T.Day -> T.Day
endOfQuarter = T.addDays (-1) . T.addGregorianMonthsClip 3 . beginningOfQuarter

endOfYear :: T.Day -> T.Day
endOfYear = T.addDays (-1) . T.addGregorianYearsClip 1 . beginningOfYear

nextWeek :: T.Day -> T.Day
nextWeek = T.addDays 7 . beginningOfWeek

nextMonth :: T.Day -> T.Day
nextMonth = T.addGregorianMonthsClip 1 . beginningOfMonth

nextQuarter :: T.Day -> T.Day
nextQuarter = T.addGregorianMonthsClip 3 . beginningOfQuarter

nextYear :: T.Day -> T.Day
nextYear = T.addGregorianMonthsClip 12 . beginningOfYear

previousWeek :: T.Day -> T.Day
previousWeek = T.addDays (-7) . beginningOfWeek

previousMonth :: T.Day -> T.Day
previousMonth = T.addGregorianMonthsClip (-1) . beginningOfMonth

previousQuarter :: T.Day -> T.Day
previousQuarter = T.addGregorianMonthsClip (-3) . beginningOfQuarter

previousYear :: T.Day -> T.Day
previousYear = T.addGregorianMonthsClip (-12) . beginningOfYear
