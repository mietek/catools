--------------------------------------------------------------------------------

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
    skipChar '/'
    m <- read <$> count 2 (satisfy isDigit)
    skipChar '/'
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
    choice
      [ istring "Jan" >> return 1
      , istring "Feb" >> return 2
      , istring "Mar" >> return 3
      , istring "Apr" >> return 4
      , istring "May" >> return 5
      , istring "Jun" >> return 6
      , istring "Jul" >> return 7
      , istring "Aug" >> return 8
      , istring "Sep" >> return 9
      , istring "Oct" >> return 10
      , istring "Nov" >> return 11
      , istring "Dec" >> return 12
      ]

--------------------------------------------------------------------------------

skipChar :: Char -> ReadP ()
skipChar c = do
    _ <- char c
    return ()

skipSpace :: ReadP ()
skipSpace =
    skipChar ' '

skipString :: String -> ReadP ()
skipString str = do
    _ <- string str
    return ()

--------------------------------------------------------------------------------

decimal :: ReadP Decimal
decimal = do
    int <- integer
    option (Decimal 0 int) $ do
      skipChar '.'
      digs <- many1 (satisfy isDigit)
      let e = fromIntegral (length digs)
          f = read digs * signum int
          d = int * 10^e + f
      return (Decimal e (d :: Integer))

--------------------------------------------------------------------------------

integer :: ReadP Integer
integer = do
    sign <- option 1 $ do
      skipChar '-'
      return (-1)
    int <- unsignedInteger
    return (int * sign)

unsignedInteger :: ReadP Integer
unsignedInteger =
    choice
      [ read <$> many1 (satisfy isDigit)
      , unsignedIntegerWithCommas
      ]

unsignedIntegerWithCommas :: ReadP Integer
unsignedIntegerWithCommas = do
    tri1 <- option "" $ do
      dig1 <- satisfy isDigit
      digs <- option [] $ do
        dig2 <- satisfy isDigit
        return [dig2]
      skipChar ','
      return (dig1 : digs)
    tris <- sepBy1 (count 3 (satisfy isDigit)) (char ',')
    return (read (concat (tri1 : tris)))

--------------------------------------------------------------------------------

istring :: String -> ReadP String
istring this = do
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

--------------------------------------------------------------------------------
