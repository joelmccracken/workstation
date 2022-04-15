-- |

module WSHS.Wrappers.Debian.Snap where

import RIO

import Turtle hiding (char, alphaNum, newline, spaces, Parser, anyChar, eof)
import qualified Data.Text as T
import Text.Trifecta
import Text.Parser.Combinators
import Text.Parser.Token

data Snap = Snap { name :: Text, version :: Text } deriving (Eq, Show)

snapListCommandHeader :: Parser ()
snapListCommandHeader =
  void $
    textSymbol "Name" <>
    textSymbol "Version" <>
    textSymbol "Rev" <>
    textSymbol "Tracking" <>
    textSymbol "Publisher" <>
    textSymbol "Notes"

snapListCommandOutputParser :: Parser [Snap]
snapListCommandOutputParser = do
  optional whiteSpace
  optional (textSymbol "snapd")

  optional $ do
    whiteSpace
    (textSymbol "snapd")
  snapListCommandHeader
  many snapItemParser

snapItemParser :: Parser Snap
snapItemParser = do
  name <- T.pack <$> many alphaNum
  void $ spaces
  version <- T.pack <$> many (alphaNum <|> char '.' <|> char '-')
  void $ manyTill anyChar ((void newline) <|> eof)
  return $ Snap name version
