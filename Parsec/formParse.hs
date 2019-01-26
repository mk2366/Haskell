import Text.ParserCombinators.Parsec
import Numeric

pHex :: CharParser () Char
pHex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d

aHex :: CharParser () Char
aHex = hexify <$> (char '%' *> hexDigit) <*> hexDigit



hexify a b = toEnum . fst . head . readHex $ [a,b]