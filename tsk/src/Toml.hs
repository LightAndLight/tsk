{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Toml
  ( readFile
  , TomlError (..)
  , Located (..)
  , Sage.ParseError (..)
  , Decoder
  , key
  , ValueDecoder
  , string
  )
where

import Control.Applicative (many, some, (<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT, get, put, runStateT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Sage (Parser)
import qualified Text.Sage as Sage
import Prelude hiding (readFile)

readFile :: FilePath -> Decoder a -> IO (Either TomlError a)
readFile path decoder = do
  input <- ByteString.readFile path
  case parse input of
    Left err ->
      pure $ Left err
    Right value ->
      pure $! decode (fmap (\(k, v) -> (k, v, False)) value) decoder

parse :: ByteString -> Either TomlError (Map Text (Located Text, Located Value))
parse input =
  case Sage.parse tomlParser input of
    Left err -> Left $ TomlParseError err
    Right a -> Right a

data TomlError
  = TomlParseError !Sage.ParseError
  | MissingKey !Text
  | UnconsumedKeys ![Located Text]
  | ValueDecodeError !(Located Text) !(Located Value)

data Located a
  = Located
      -- | Offset
      !Int
      -- | Value
      a

instance Eq a => Eq (Located a) where
  Located _ a == Located _ b = a == b

instance Ord a => Ord (Located a) where
  Located _ a `compare` Located _ b = a `compare` b

located :: Parser a -> Parser (Located a)
located p = Located <$> Sage.getOffset <*> p

newtype Value
  = String Text

tomlParser :: Parser (Map Text (Located Text, Located Value))
tomlParser =
  Map.fromList
    <$> Sage.sepBy
      ( (\k'@(Located _ k) v -> (k, (k', v)))
          <$> located keyParser
          <* token (Sage.char '=')
          <*> located value
      )
      (Sage.skipSome newline)
  where
    whitespace = Sage.char ' ' <|> Sage.char '\t'
    newline = void (Sage.char '\n') <|> void (Sage.string $ Text.pack "\r\n")

    token p = p <* Sage.skipMany whitespace

    keyParser =
      token $
        Text.pack <$> some (Sage.satisfy ((||) <$> Char.isAlphaNum <*> (`elem` "_-")))

    value =
      String <$> stringParser

    stringParser =
      fmap Text.pack $
        Sage.char '"'
          *> many (Sage.satisfy (`notElem` escapes) <|> Sage.char '\\' *> Sage.satisfy (`elem` escapes))
          <* Sage.char '"'

    escapes = "\\\""

newtype Decoder a
  = Decoder (StateT (Map Text (Located Text, Located Value, Bool)) (Either TomlError) a)
  deriving (Functor, Applicative)

key :: Text -> ValueDecoder a -> Decoder a
key k (ValueDecoder f) =
  Decoder $ do
    kvs <- get
    case Map.lookup k kvs of
      Nothing ->
        throwError $ MissingKey k
      Just (k', value'@(Located _offset value), _consumed) ->
        case f value of
          Nothing ->
            throwError $ ValueDecodeError k' value'
          Just a -> do
            put $ Map.insert k (k', value', True) kvs
            pure a

decode :: Map Text (Located Text, Located Value, Bool) -> Decoder a -> Either TomlError a
decode input (Decoder d) =
  case runStateT d input of
    Left err -> Left err
    Right (a, output) -> do
      let unconsumed = Map.mapMaybe (\(k, v, consumed) -> if consumed then Nothing else Just (k, v)) output
      if Map.null unconsumed
        then pure a
        else Left . UnconsumedKeys . fmap fst $ Map.elems unconsumed

newtype ValueDecoder a = ValueDecoder (Value -> Maybe a)
  deriving (Functor, Applicative) via (ReaderT Value Maybe)

string :: ValueDecoder Text
string = ValueDecoder (\case String s -> Just s)
