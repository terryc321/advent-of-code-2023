
import Data.Array as Array
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative hiding (many)

type Point = (Int, Int, Int)

-- Parses a single 3D point like 1,2,3
pointParser :: Parser Point
pointParser = do
  x <- int
  _ <- char ','
  y <- int
  _ <- char ','
  z <- int
  return (x, y, z)
  where
    int = read <$> many1 digit

-- Parses a line like 1,2,3~4,5,6
lineParser :: Parser (Point, Point)
lineParser = do
  p1 <- pointParser
  _ <- char '~'
  p2 <- pointParser
  return (p1, p2)

-- Parses multiple lines ending in newline
linesParser :: Parser [(Point, Point)]
linesParser = endBy lineParser newline

-- Run the parser
parseInput :: String -> Either ParseError [(Point, Point)]
parseInput = parse linesParser ""

-- Example usage
exampleInput :: String
exampleInput = unlines
  [ "1,0,1~1,2,1"
  , "0,0,2~2,0,2"
  , "0,2,3~2,2,3"
  , "0,0,4~0,2,4"
  , "2,0,5~2,2,5"
  , "0,1,6~2,1,6"
  , "1,1,8~1,1,9"
  ]

-- lines exampleInput does opposite

examplePoints = [ ((1,0,1),(1,2,1)),
                  ((0,0,2),(2,0,2)),
                  ((0,2,3),(2,2,3)),
                  ((0,0,4),(0,2,4)),
                  ((2,0,5),(2,2,5)),
                  ((0,1,6),(2,1,6)),
                  ((1,1,8),(1,1,9))]

main :: IO ()
main = do
  case parseInput exampleInput of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right result -> mapM_ print result
