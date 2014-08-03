--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse.Common where

import Control.Applicative ((<$>))
import Data.Char (isDigit, toLower)
import Data.Decimal (Decimal, DecimalRaw (Decimal))
import Data.Time.Calendar (Day, fromGregorianValid)

import Text.ParserCombinators.ReadP

--------------------------------------------------------------------------------

visaDate :: ReadP Day
visaDate = do
    d <- read <$> count 2 (satisfy isDigit)
    char '/'
    m <- read <$> count 2 (satisfy isDigit)
    char '/'
    shortY <- read <$> count 2 (satisfy isDigit)
    let y = if shortY < 70 then 2000 + shortY else 1900 + shortY
    maybe pfail return (fromGregorianValid y m d)

--------------------------------------------------------------------------------

shortDate :: ReadP Day
shortDate = do
    d <- read <$> count 2 (satisfy isDigit)
    m <- shortMonth
    y <- read <$> count 4 (satisfy isDigit)
    maybe pfail return (fromGregorianValid y m d)

shortMonth :: ReadP Int
shortMonth =
    leftBiasedChoice
      [ "Jan" ~> 1
      , "Feb" ~> 2
      , "Mar" ~> 3
      , "Apr" ~> 4
      , "May" ~> 5
      , "Jun" ~> 6
      , "Jul" ~> 7
      , "Aug" ~> 8
      , "Sep" ~> 9
      , "Oct" ~> 10
      , "Nov" ~> 11
      , "Dec" ~> 12
      ]

--------------------------------------------------------------------------------

decimal :: ReadP Decimal
decimal = do
    n <- integer
    option (Decimal 0 n) $ do
      char '.'
      digs <- munch1 isDigit
      let s = if signum n == (-1) then (-1) else 1
          e = length digs
          f = read digs
          d = n * 10 ^ e + (f * s)
      return (Decimal (fromIntegral e) d)

--------------------------------------------------------------------------------

integer :: ReadP Integer
integer = do
    s <- option 1 $ do
      char '-'
      return (-1)
    n <- read <$> commaSepTriplets <++ munch1 isDigit
    return (n * s)

commaSepTriplets :: ReadP String
commaSepTriplets = do
    tri1 <- option "" $ do
      dig1 <- satisfy isDigit
      digs <- option [] $ do
        dig2 <- satisfy isDigit
        return [dig2]
      char ','
      return (dig1 : digs)
    tris <- sepBy1 (count 3 (satisfy isDigit)) (char ',')
    return (concat (tri1 : tris))

--------------------------------------------------------------------------------

caseInsensitiveString :: String -> ReadP String
caseInsensitiveString this = do
    s <- look
    scan this s
  where
    scan [] _ =
        return this
    scan (x : xs) (y : ys)
        | toLower x == toLower y =
            get >> scan xs ys
    scan _ _
        = pfail

(~>) :: String -> a -> ReadP a
str ~> val = do
    caseInsensitiveString str
    return val

--------------------------------------------------------------------------------

leftBiasedChoice :: [ReadP a] -> ReadP a
leftBiasedChoice [] =
    pfail
leftBiasedChoice [p] =
    p
leftBiasedChoice (p : ps) =
    p <++ leftBiasedChoice ps

space :: ReadP Char
space =
    char ' '

--------------------------------------------------------------------------------
