module App.DateTime (

    module App.DateTime
  , module Data.DateTime
  , module Data.DateTime.Instant
  , module Data.DateTime.Locale
  ) where

import Data.Maybe
import Data.DateTime
import Data.DateTime.Instant
import Data.DateTime.Locale
import Data.Either
import Data.Function.Uncurried
import Data.Time.Duration as Duration
import Data.JSDate as JSD
import Data.Formatter.DateTime
import Prelude

import Debug.Trace

localeToDateTime :: LocalDateTime -> DateTime
localeToDateTime (LocalValue l d) = d


--foreign import data JSDate :: *
foreign import parseDateStringImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe Number)

parseDateString :: String -> Maybe Number
parseDateString str = runFn3 parseDateStringImpl Just Nothing str

parseDateTime :: String -> Maybe DateTime
parseDateTime s = toDateTime <$> (join $ instant <$> Duration.Milliseconds <$> spy <$> parseDateString s)
--parseDateTime s = toDateTime <$> (join $ instant <$> Duration.Milliseconds <$> Just 3453453453.3)

shortDateTime :: DateTime -> String
shortDateTime d = case formatDateTime "YYYY MMM DD HH:mm:ss" d of
                    Left _ -> "invalid date format"
                    Right s -> s

{-
parseLocalDateTime :: String -> Eff Maybe LocalDateTime
parseLocalDateTime s = do
  --x <- JSD.parse s
  pure Nothing
-}

{-
parseLocalDateTime :: String -> Maybe LocalDateTime
parseLocalDateTime s = case FDT.parseFormatString "DD-MM-YYYY HH-:-mm" of 
                    Left _ -> Nothing
                    Right fmtr -> case FDT.unformat fmtr s of
                      Left _ -> Nothing
                      Right dt -> Just dt
-}
{-
parseLocalDateTime :: String -> LocalDateTime
parseLocalDateTime s = JSD.parse s

readLocalDateTime  ::  Foreign -> F LocalDateTime
— TODO

readLocalDateTimeJSON :: String -> F LocalDateTime
readLocalDateTimeJSON = parseJSON >=> readLocalDateTime
-}