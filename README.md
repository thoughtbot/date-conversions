# Date Conversions

This is a Haskell library for handling date operations easily, e.g. calculating
the beginning or end of a week, month, quarter, or year.

```haskell
import qualified Data.Time as T
import qualified Data.Time.Calendar.DateConversions as DC

-- Given the following signatures
--
-- now :: T.UTCTime

DC.beginningOfWeek $ T.utctDay now
DC.beginningOfMonth $ T.utctDay now
DC.endOfYear $ T.utctDay now
```

## Hackage

This package is available on [Hackage].

[Hackage]: http://hackage.haskell.org/package/date-conversions

## License

DateConversions is Copyright © 2016-2017 Josh Clayton and thoughtbot, inc. It
is free software, and may be redistributed under the terms specified in the
[LICENSE](/LICENSE) file.

## About thoughtbot

![thoughtbot](https://thoughtbot.com/logo.png)

DateConversions is maintained and funded by thoughtbot, inc.
The names and logos for thoughtbot are trademarks of thoughtbot, inc.

We love open source software and Haskell. See [our other Haskell
projects][haskell-services], or [hire our Haskell development team][hire]
to design, develop, and grow your product.

[haskell-services]: https://thoughtbot.com/services/haskell?utm_source=github
[hire]: https://thoughtbot.com?utm_source=github
