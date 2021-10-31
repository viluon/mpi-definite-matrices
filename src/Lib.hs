module Lib
( main
) where

import Prelude hiding (lex)
import Control.Applicative (Alternative((<|>)))
import Numeric.LinearAlgebra
    (realPart, Complex(..), eigenvalues, (><), toList, Matrix)
import Data.Attoparsec.Text
    (many', endOfInput, double, skipSpace, parseOnly, Parser)
import Data.Text (pack)

main :: IO ()
main = do
  input <- getContents
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
  foldl ((. uncurry classify) <$> (<|>)) Nothing classes

  where
    eValues = realPart <$> toList (eigenvalues m)
    classify f c = if all f eValues then Just c else Nothing

    orElse Nothing  x = x
    orElse (Just x) _ = x

    classes = [ ((>  0), "positive definite")
              , ((<  0), "negative definite")
              , ((>= 0), "positive semidefinite")
              , ((<= 0), "negative semidefinite")
              ]
