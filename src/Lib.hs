module Lib
( main
) where

import Prelude hiding (lex)
import Numeric.LinearAlgebra hiding (double)
import Data.Attoparsec.Text
import Data.Text (pack)

main :: IO ()
main = do
  input <- getContents
  print input
  let result = parseOnly (matrixParser <* endOfInput) $ pack input
  putStrLn $ case result of
    Left  err -> err
    Right m   -> determineDefiniteness m

matrixParser :: Parser (Matrix (Complex Double))
matrixParser = loadMatrix =<< (skipSpace *> many' (double <* skipSpace))
  where
    loadMatrix xs = do
      let n = floor . sqrt . fromIntegral . length $ xs
      if n * n /= length xs
      then fail "The provided matrix has to be square"
      else pure $ (n><n) $ (:+ 0) <$> xs

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
