{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
#-}

module Main where
import System.Environment
import System.Directory
import Control.Monad
import Utilities
import Text.ParserCombinators.Parsec
import System.IO  
import Text.Parsec.Language (emptyDef)
import Data.Char

--delimToList:: a->[a]->[[a]]
--delimToList 
--
--delimToList'::([[a]],[a],a) -> [a] -> [a]
--delimToList' z y x = delimToList 
--
--untilSpace:: String -> (String, String)
--untilSpace x = untilSpace ("", x)
--
--untilSpace':: (String, String) -> (String, String)
--untilSpace' (a,b) = 
--  case b of 
--    ""   -> (a,"")
--    " ":xs -> (a,xs)
--    x:xs -> untilSpace' (a:x,xs)

parseCommands::[(String, String)] -> Parser [(String,String)]
parseCommands li = 
  do {eof; return li}
    <|> (try (do {
        many (oneOf "\n");
        arg1 <- many1 (noneOf " ");
        spaces;
        arg2 <- many1 (noneOf "\n");
        parseCommands ((arg1,arg2):li)
        --parses eol automatically?
      }))
    <|> (return li)

readCommands::String -> [(String,String)]
readCommands s =
  justRight (parse (parseCommands []) "error" s)

formatPairs :: String -> ((String,String) -> String) -> [(String,String)] -> String
formatPairs header f arr = header ++ (concat (map f arr))

pairsToEnvVars :: [(String, String)] -> String
pairsToEnvVars arr = formatPairs "#!/bin/bash\n\n" (\(x,y) -> "setx X"++ (map toUpper x) ++ " \"" ++ y ++ "\" /M\n") arr

pairsToCommands::[(String, String)] -> String
pairsToCommands arr = formatPairs "#!/bin/bash\n\n" (\(x,y) -> "if [[ $1 == "++x++" ]]; then\n    cygstart \""++y++"\"\nfi\n") arr
{-  let 
    arr = readCommands s
  in
    "#!/bin/bash\n\n"++(concat (map (\(x,y) -> "if [[ $1 == "++x++" ]]; then\n    cygstart \""++y++"\"\nfi\n") arr))-}

ioFile:: String -> String -> (String -> String) -> IO ()
ioFile inputF outputF f =
 do  
  handle <- openFile inputF ReadMode
  contents <- hGetContents handle
  writeFile outputF (f contents)

iFile :: String -> (String -> a) -> IO a
iFile inputF f = 
    do  
      handle <- openFile inputF ReadMode
      contents <- hGetContents handle
      return (f contents)

main::IO ()
main = do
  args <- getArgs
  let a0 = if (length args == 0) then "in.txt" else (args!!0)
  let a1 = if (length args <= 1) then "out.txt" else (args!!1)
  arr <- iFile a0 readCommands
  writeFile a1 (pairsToCommands arr)
  writeFile "setenv" (pairsToEnvVars arr)
--  ioFile a0 a1 commandsToShell
