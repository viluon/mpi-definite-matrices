module Lib
( main
) where

import Numeric.LinearAlgebra
import Text.ParserCombinators.ReadP
import Text.Read.Lex

main :: IO ()
main = do
  input <- getContents
  let [(m, "")] = filter (null . snd) $ readP_to_S parse input
  putStrLn $ determineDefiniteness m

parse :: ReadP (Matrix (Complex Double))
parse = loadMatrix <$> many (between skipSpaces skipSpaces readDecP)
  where
    loadMatrix xs =
      let n = floor . sqrt . fromIntegral . length $ xs
      in (n><n) xs

determineDefiniteness :: Matrix (Complex Double) -> String
determineDefiniteness m = flip orElse "indefinite" $
  let eValues = realPart <$> toList (eigenvalues m)
  in foldl (\acc (f, c) -> if all f eValues then Just $ acc `orElse` c else acc) Nothing classes

  where
    orElse Nothing  x = x
    orElse (Just x) _ = x

    classes = [ ((>  0), "positive definite")
              , ((<  0), "negative definite")
              , ((>= 0), "positive semidefinite")
              , ((<= 0), "negative semidefinite")
              ]
