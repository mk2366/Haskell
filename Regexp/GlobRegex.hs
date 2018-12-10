module GlobRegex (
    globToRegex,
    matchesGlob
) where

import Text.Regex.TDFA ((=~))

globToRegex :: String -> String
globToRegex gs  = '^' : globToRegex' gs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.'  :  globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = "[" ++ c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs)  = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
         where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
file `matchesGlob` pat = file =~ globToRegex pat
