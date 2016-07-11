{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (toLower, toUpper)
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
  fixCase toLower ty `mappend` fixCase toUpper (rename i) `mappend` " :: " `mappend` fieldType `mappend` " -- ^ " `mappend` d
  where
    fixCase f ty =
      let first = T.head ty
      in T.singleton (f first) `mappend` T.drop 1 ty

    fieldType =
      if nullableField ty i
      then "Maybe " `mappend` hsType t
      else hsType t

rename :: T.Text -> T.Text
rename "default" = "default'"
rename "type" = "type'"
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

nullableField :: T.Text -> T.Text -> Bool
nullableField "EvolutionChain" "baby_trigger_item" = True
nullableField "EvolutionDetail" "min_beauty" = True
nullableField "EvolutionDetail" "gender" = True
nullableField "EvolutionDetail" "relative_physical_stats" = True
nullableField "EvolutionDetail" "item" = True
nullableField "EvolutionDetail" "known_move_type" = True
nullableField "EvolutionDetail" "min_affection" = True
nullableField "EvolutionDetail" "party_type" = True
nullableField "EvolutionDetail" "trade_species" = True
nullableField "EvolutionDetail" "party_species" = True
nullableField "EvolutionDetail" "min_happiness" = True
nullableField "EvolutionDetail" "held_item" = True
nullableField "EvolutionDetail" "known_move" = True
nullableField "EvolutionDetail" "location" = True
nullableField "PokemonSpecies" "evolves_from_species" = True
nullableField "PokemonSprites" "front_female" = True
nullableField "PokemonSprites" "front_shiny_female" = True
nullableField "PokemonSprites" "back_female" = True
nullableField "PokemonSprites" "back_shiny_female" = True
nullableField _ _ = False

generateFromJsonInstance :: T.Text -> [Field] -> IO ()
generateFromJsonInstance ty flds = do
  T.putStrLn $ "instance FromJSON " `mappend` ty `mappend` " where"
  T.putStrLn $ "  parseJSON (Object o) ="
  T.putStrLn $ "    " `mappend` ty
  T.putStr "      <$> "
  let vs = intersperse "<*>" $
             map (\field -> "o .: \"" `mappend` identifier field `mappend` "\"") flds
  mapM_ (\x -> if x == "<*>" then T.putStr ("      <*> ") else T.putStrLn x) vs
  T.putStrLn $ "  parseJSON _ = mzero\n"

export :: (T.Text, [T.Text]) -> IO ()
export (ty', tbl') = do
  let ty = renameType ty'
  let tbl = drop 2 tbl'
  let fields = map processTableLine tbl
  T.putStrLn $ "data " `mappend` ty
  T.putStrLn $ "  = " `mappend` ty `mappend` " {"
  T.putStr "      "
  let stuff = intersperse "," $ map (\x -> fieldText ty $ x) fields
  mapM_ (\x -> if x == "," then T.putStr ("    , ") else T.putStrLn x) stuff
  T.putStrLn $ "  } deriving (Eq, Ord, Show)\n"
  generateFromJsonInstance ty fields

header :: IO ()
header = do
  putStrLn "{-# LANGUAGE TemplateHaskell #-}"
  putStrLn "{-# LANGUAGE MultiParamTypeClasses #-}"
  putStrLn "{-# LANGUAGE FunctionalDependencies #-}"
  putStrLn "{-# LANGUAGE FlexibleInstances #-}"
  putStrLn "{-# LANGUAGE OverloadedStrings #-}"
  putStrLn ""
  putStrLn "-- NOTE! This module is automatically generated!"
  putStrLn "module Games.Pokemon.Pokeapi.Types where"
  putStrLn "import Data.Aeson"
  putStrLn "import Control.Lens"
  putStrLn "import Control.Monad (mzero)"
  putStrLn ""

makeFields :: T.Text -> IO ()
makeFields ty = T.putStrLn $ "makeFields ''" `mappend` ty

main :: IO ()
main = do
  input <- head <$> getArgs
  text <- T.lines <$> T.readFile input
  let src = drop 1 $ findTypes text [("", [])]
  header
  mapM_ export src
  mapM_ (makeFields . renameType . fst) src
