{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Bind

-- $setup
-- >>> :set -XOverloadedStrings

-- A data type representing the digits zero to nine.
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Enum, Bounded, Show)

-- A data type representing one, two or three digits, which may be useful for grouping.
data DigitGroup =
    D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars :: Chars -> Chars
dollars input =
  let (inleft, inright) = break ('.' ==) input
      (ds, cs) = (toDigits inleft, (take 2 . padCents . toDigits) inright)
  in showDollars ds ++ " and " ++ showCents cs
  where 
    showDollars (One:.Nil) = "one dollar"
    showDollars        xs  = showDigits xs ++ "dollars"
    showCents (Zero:.Zero:.Nil) = "zero cents"
    showCents (Zero:.One :.Nil) = "one cent"
    showCents               xs  = showDigits xs ++ "cents"
    padCents (x:.Nil) = x:.Zero:.Nil
    padCents       x  = x

toDigits :: Chars -> List Digit
toDigits cs = let ds = listOptional fromChar cs in if isEmpty ds then Zero:.Nil else ds

showDigits ::  List Digit -> Chars
showDigits = unwords . toIllion . groupDigits
  where
    toIllion = reverse . filter (/= "") . flip (zipWith toChars) illion . reverse
    toChars (D2 Zero Zero)      _ = ""
    toChars (D3 Zero Zero Zero) _ = ""
    toChars dGroup suffix = digitGroupToChars dGroup ++ " " ++ suffix

groupDigits ::  List Digit -> List DigitGroup
groupDigits Nil = Nil
groupDigits xs = 
  let y:.ys = reverse xs
  in uncurry (:.) (foldLeft step (D1 y, Nil) ys)
  where 
    step (D1 d1   , acc) a = (D2 a d1, acc)
    step (D2 d1 d2, acc) a = (D3 a d1 d2, acc)
    step (d@D3{}  , acc) a = (D1 a, d:.acc) 

digitGroupToChars :: DigitGroup -> Chars
digitGroupToChars = \case
  D1           d     -> showDigit d
  D2      Zero d     -> showDigit d
  D2      One  Zero  -> "ten"
  D2      One  One   -> "eleven"
  D2      One  Two   -> "twelve"
  D2      One  Three -> "thirteen"
  D2      One  Four  -> "fourteen"
  D2      One  Five  -> "fifteen"
  D2      One  Six   -> "sixteen"
  D2      One  Seven -> "seventeen"
  D2      One  Eight -> "eighteen"
  D2      One  Nine  -> "nineteen"
  D2      t    Zero  -> tensToChars t
  D2      t    d     -> tensToChars t ++ "-" ++ showDigit d
  D3 Zero t    d     -> digitGroupToChars (D2 t d)
  D3 h    Zero Zero  -> showDigit h ++ " hundred"
  D3 h    t    d     -> showDigit h ++ " hundred and " ++ digitGroupToChars (D2 t d)


tensToChars :: Digit -> Chars
tensToChars = \case
  Zero  -> ""
  One   -> "teen"
  Two   -> "twenty"
  Three -> "thirty"
  Four  -> "forty"
  Five  -> "fifty"
  Eight -> "eighty"
  d     -> showDigit d ++ "ty"

showDigit :: Digit -> Chars
showDigit = \case
 Zero  -> "zero"
 One   -> "one"
 Two   -> "two"
 Three -> "three"
 Four  -> "four"
 Five  -> "five"
 Six   -> "six"
 Seven -> "seven"
 Eight -> "eight"
 Nine  -> "nine"

-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar = \case
 '0' -> Full Zero
 '1' -> Full One
 '2' -> Full Two
 '3' -> Full Three
 '4' -> Full Four
 '5' -> Full Five
 '6' -> Full Six
 '7' -> Full Seven
 '8' -> Full Eight
 '9' -> Full Nine
 _ -> Empty

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion :: List Chars
illion =
  let preillion = listh [ const "" , const "un" , const "do" , const "tre" , const "quattuor" , const "quin" , const "sex" , const "septen" , const "octo" , \q -> if "n" `isPrefixOf` q then "novem" else "noven" ]
      postillion = listh [ "vigintillion" , "trigintillion" , "quadragintillion" , "quinquagintillion" , "sexagintillion" , "septuagintillion" , "octogintillion" , "nonagintillion" , "centillion" , "decicentillion" , "viginticentillion" , "trigintacentillion" , "quadragintacentillion" , "quinquagintacentillion" , "sexagintacentillion" , "septuagintacentillion" , "octogintacentillion" , "nonagintacentillion" , "ducentillion" , "deciducentillion" , "vigintiducentillion" , "trigintaducentillion" , "quadragintaducentillion" , "quinquagintaducentillion" , "sexagintaducentillion" , "septuagintaducentillion" , "octogintaducentillion" , "nonagintaducentillion" , "trecentillion" , "decitrecentillion" , "vigintitrecentillion" , "trigintatrecentillion" , "quadragintatrecentillion" , "quinquagintatrecentillion" , "sexagintatrecentillion" , "septuagintatrecentillion" , "octogintatrecentillion" , "nonagintatrecentillion" , "quadringentillion" , "deciquadringentillion" , "vigintiquadringentillion" , "trigintaquadringentillion" , "quadragintaquadringentillion" , "quinquagintaquadringentillion" , "sexagintaquadringentillion" , "septuagintaquadringentillion" , "octogintaquadringentillion" , "nonagintaquadringentillion" , "quingentillion" , "deciquingentillion" , "vigintiquingentillion" , "trigintaquingentillion" , "quadragintaquingentillion" , "quinquagintaquingentillion" , "sexagintaquingentillion" , "septuagintaquingentillion" , "octogintaquingentillion" , "nonagintaquingentillion" , "sescentillion" , "decisescentillion" , "vigintisescentillion" , "trigintasescentillion" , "quadragintasescentillion" , "quinquagintasescentillion" , "sexagintasescentillion" , "septuagintasescentillion" , "octogintasescentillion" , "nonagintasescentillion" , "septingentillion" , "deciseptingentillion" , "vigintiseptingentillion" , "trigintaseptingentillion" , "quadragintaseptingentillion" , "quinquagintaseptingentillion" , "sexagintaseptingentillion" , "septuagintaseptingentillion" , "octogintaseptingentillion" , "nonagintaseptingentillion" , "octingentillion" , "decioctingentillion" , "vigintioctingentillion" , "trigintaoctingentillion" , "quadragintaoctingentillion" , "quinquagintaoctingentillion" , "sexagintaoctingentillion" , "septuagintaoctingentillion" , "octogintaoctingentillion" , "nonagintaoctingentillion" , "nongentillion" , "decinongentillion" , "vigintinongentillion" , "trigintanongentillion" , "quadragintanongentillion" , "quinquagintanongentillion" , "sexagintanongentillion" , "septuagintanongentillion" , "octogintanongentillion" , "nonagintanongentillion" ]
  in listh [ "" , "thousand" , "million" , "billion" , "trillion" , "quadrillion" , "quintillion" , "sextillion" , "septillion" , "octillion" , "nonillion" , "decillion" , "undecillion" , "duodecillion" , "tredecillion" , "quattuordecillion" , "quindecillion" , "sexdecillion" , "septendecillion" , "octodecillion" , "novemdecillion" ] 
     ++ lift2 ((++) =<<) preillion postillion
