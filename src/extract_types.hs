{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (toLower)
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

findNextType :: [T.Text] -> [T.Text]
findNextType = dropWhile (\line -> not ("#### " `T.isPrefixOf` line))

-- | Given @#### Foo Bar@, returns @FooBar@.
extractType :: T.Text -> T.Text
extractType = T.filter noSpaces . T.drop 5
  where noSpaces x = x /= ' ' && x /= '\n'

extractTable :: [T.Text] -> ([T.Text], [T.Text])
extractTable x =
  let tbl = takeWhile (\l -> ("|" `T.isPrefixOf` l)) . tail $ x
  in (tbl, drop (length tbl + 1) x)

findTypes :: [T.Text] -> [(T.Text, [T.Text])] -> [(T.Text, [T.Text])]
findTypes src acc =
  case findNextType src of
  [] -> acc
  (t:leftover) -> let (extracted, remaining) = extractTable leftover
                  in findTypes remaining (acc ++ [(extractType t, extracted)])

data Field = Field { identifier :: T.Text, desc :: T.Text, type' :: T.Text }

processTableLine :: T.Text -> Field
processTableLine line =
  let (_:a:b:c:_) = T.splitOn "|" line
  in Field (T.strip a) (T.strip b) (T.strip c)

fieldText :: T.Text -> Field -> T.Text
fieldText ty (Field i d t) =
  fixCase ty `mappend` "_" `mappend` rename i `mappend` " :: " `mappend` hsType t `mappend` " -- ^ " `mappend` d
  where fixCase ty =
          let first = T.head ty
          in T.singleton (toLower first) `mappend` T.drop 1 ty

rename :: T.Text -> T.Text
rename "default" = "default'"
rename x = x

hsType :: T.Text -> T.Text
hsType "integer" = "Integer"
hsType "string" = "String"
hsType "boolean" = "Bool"
hsType s
  | "[NamedAPIResource]" `T.isPrefixOf` s = "NamedAPIResource"
  | "list [" `T.isPrefixOf` s = (T.takeWhile (/= ']') (T.drop 5 s)) `mappend` "]"
  | "list " `T.isPrefixOf` s = "[" `mappend` hsType (T.takeWhile (/= ' ') (T.drop 5 s)) `mappend` "]"
  | " (" `T.isInfixOf` s = hsType (T.takeWhile (/= ' ') s)
  | "[" `T.isPrefixOf` s = T.takeWhile (/= ']') (T.drop 1 s)
  | otherwise = T.takeWhile (/= ' ') s

renameType :: T.Text -> T.Text
renameType x
  | "<aid=\"resourcename\"></a>" `T.isPrefixOf` x = T.drop 24 x
  | otherwise = x

export :: (T.Text, [T.Text]) -> IO ()
export (ty', tbl) = do
  let ty = renameType ty'
  T.putStrLn $ "data " `mappend` ty
  T.putStrLn $ "  = " `mappend` ty `mappend` " {"
  T.putStr "      "
  let stuff = intersperse "," $ map (\x -> fieldText ty . processTableLine $ x) (drop 2 tbl)
  mapM_ (\x -> if x == "," then T.putStr ("    , ") else T.putStrLn x) stuff
  T.putStrLn $ "  } deriving (Eq, Ord, Show)\n"

header :: IO ()
header = do
  putStrLn "-- NOTE! This module is automatically generated!"
  putStrLn "module Games.Pokemon.Pokeapi.Types where"

main :: IO ()
main = do
  input <- head <$> getArgs
  text <- T.lines <$> T.readFile input
  let src = drop 1 $ findTypes text [("", [])]
  header
  mapM_ export src
