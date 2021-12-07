import Text.Parsec

parser :: Parsec String () [Integer]
parser =
  do
    nums <- number `sepBy` char ','
    _ <- newline
    return nums

number :: Parsec String () Integer
number = read <$> many1 digit

-- part 1 = finding the median
-- part 2 = found the min value by just iterating over all the points from [0, 2000]