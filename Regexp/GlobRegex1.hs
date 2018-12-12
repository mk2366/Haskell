module GlobRegex (
    globToRegex,
    matchesGlob
) where

import Text.Regex.TDFA ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex gs  = globToRegex' gs >>= (\s -> Right ('^' : s ++ "$"))

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""
globToRegex' ('*':cs) = globToRegex' cs >>= (\s -> Right (".*" ++ s))
globToRegex' ('?':cs) = globToRegex' cs >>= (\s -> Right ('.' : s))
globToRegex' ('[':'!':c:cs) = charClass cs >>= (\s -> Right ("[^" ++ c :s))
globToRegex' ('[':c:cs) = charClass cs >>= (\s -> Right ("[" ++ c :s))
globToRegex' ('[':_) = Left "unterminated character class"
globToRegex' (c:cs)  = globToRegex' cs >>= (\s -> Right (escape c ++ s))

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
         where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']':cs) = globToRegex' cs >>= (\s -> Right (']':s))
charClass (c:cs)   = charClass cs >>= (\s -> Right (c:s))
charClass []       = Left "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
file `matchesGlob` pat = file =~ globToRegex pat
