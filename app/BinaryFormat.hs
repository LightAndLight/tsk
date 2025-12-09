{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import BinaryCodec (Def, Ty (..), docDocumented, documentedDef, runDocumented)
import Control.Applicative ((<**>))
import Data.Foldable (for_, toList)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as LazyText
import qualified Options.Applicative as Options
import qualified Pretty
import System.IO (hPutStrLn, stderr)
import qualified Todo.V0
import Data.List.NonEmpty (NonEmpty (..))
import GHC.IsList (fromList)
import Prelude hiding (break)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Writer (Writer, runWriter, tell)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text.Lazy (LazyText)
import qualified Data.Text.Lazy as LazyText

data Cli
  = CliVersion {version :: Text}
  | CliGrammar

cliParser :: Options.Parser Cli
cliParser =
  Options.hsubparser
    ( Options.command
        "version"
        ( Options.info
            ( ( CliVersion
                  <$> Options.strArgument
                    ( Options.metavar "NUMBER" <> Options.help "Binary format version to print"
                    )
              )
                <**> Options.helper
            )
            Options.fullDesc
        )
        <> Options.command
          "grammar"
          ( Options.info
              (pure CliGrammar <**> Options.helper)
              Options.fullDesc
          )
    )

data SomeCodec = forall a. SomeCodec (Def (Codec a))

versions :: [(Text, SomeCodec)]
versions = [(fromString "0", SomeCodec Todo.V0.state)]

data Production
  = Production
      -- | Name
      !Text
      (Maybe ProductionValue)
  | Break

data ProductionValue
  = Literal !Text
  | Name !Text
  | Or (NonEmpty ProductionValue)
  | Sequence (NonEmpty ProductionValue)
  | Class (NonEmpty (Char, Char))
  | NotClass (NonEmpty (Char, Char))
  | Optional ProductionValue
  | Many ProductionValue
  | Some ProductionValue

newtype Reference = Reference Text

ref :: Reference -> ProductionValue
ref (Reference r) = Name r

newtype Grammar a = Grammar (Writer [Production] a)
  deriving (Functor, Applicative, Monad, MonadFix)

definitions :: Grammar () -> [Production]
definitions (Grammar ma) = let ((), ps) = runWriter ma in ps

define :: String -> [ProductionValue] -> Grammar Reference
define (fromString -> name) (NonEmpty.nonEmpty -> values) =
  Grammar $ do
    tell [Production name $ fmap Sequence values]
    pure $ Reference name

lit :: String -> ProductionValue
lit = Literal . fromString

break :: Grammar ()
break = Grammar $ tell [Break]

grammar :: [Production]
grammar =
  definitions $ mdo
    _primitiveDefinition <-
      define
        "primitive-definition"
        [ lit "primitive"
        , ref typeName
        , Many $ ref name
        , lit "is"
        , ref string
        ]

    break

    _typeDefinition <-
      define
        "type-definition"
        [ lit "type"
        , ref typeName
        , Many $ ref name
        , lit "="
        , ref type_
        ]

    break

    typeName <-
      define
        "type-name"
        [ Class $ fromList [('A', 'Z')]
        , Many . Class $ fromList [('A', 'Z'), ('a', 'z'), ('0', '9')]
        ]
    name <-
      define
        "name"
        [ Class $ fromList [('a', 'z')]
        , Many . Class $ fromList [('A', 'Z'), ('a', 'z'), ('0', '9')]
        ]

    break

    type_ <-
      define
        "type"
        [ Or $ fromList [ref typeApplication, ref typeMatch]
        ]
    typeApplication <-
      define
        "type-application"
        [ ref typeAtom
        , Many $ ref typeAtom
        ]
    typeAtom <-
      define
        "type-atom"
        [ Or $ fromList
            [ ref typeRecord
            , ref typeLiteral
            , Sequence $ fromList [lit "(", ref type_, lit ")"]
            ]
        ]

    break

    typeRecord <-
      define
        "type-record"
        [ lit "{"
        , Optional . Sequence $
            fromList
              [ ref typeRecordItem
              , Many . Sequence $ fromList [lit ",", ref typeRecordItem]
              ]
        , lit "}"
        ]
    typeRecordItem <-
      define
        "type-record-item"
        [ ref name, lit ":", ref type_
        ]

    break

    typeLiteral <-
      define
        "type-literal"
        [ Or $ fromList [ref int, ref string]
        ]
    int <-
      define
        "int"
        [Optional $ lit "-", Some . Class $ fromList [('0', '9')]]
    string <-
      define
        "string"
        [ lit "\""
        , Many . Or $
            fromList
              [ NotClass $ fromList [('"', '"')]
              , Sequence $ fromList [lit "\\", lit "\""]
              ]
        , lit "\""
        ]

    break

    typeMatch <-
      define
        "type-match"
        [ lit "match"
        , ref type_
        , lit "{"
        , Optional . Sequence $
          fromList
            [ ref typeMatchItem
            , Many . Sequence $ fromList [lit ",", ref typeMatchItem]
            ]
        , lit "}"
        ]
    typeMatchItem <-
      define
        "type-match-item"
        [ ref typeMatchPattern
        , lit "=>"
        , ref type_
        ]
    typeMatchPattern <-
      define
        "type-match-pattern"
        [ ref typeLiteral ]

    pure ()

renderProduction :: Production -> LazyText
renderProduction Break = fromString "\n"
renderProduction (Production name values) =
  fromString "<a id=\"grammar-" <> LazyText.fromStrict name <>
  fromString "\" href=\"#grammar-" <> LazyText.fromStrict name <> fromString "\">" <>
  LazyText.fromStrict name <>
  fromString "</a>" <>
  fromString " ::= " <>
  foldMap renderProductionValue values <> fromString "\n"
  where
    renderClass (a, b)
      | a == b = fromString [a]
      | otherwise = fromString [a, '-', b]

    parens v = fromString "(" <> v <> fromString ")"

    renderProductionValue :: ProductionValue -> LazyText
    renderProductionValue value =
      case value of
        Name n ->
          fromString "<a href=\"#grammar-" <> LazyText.fromStrict n <> fromString "\">" <>
          fromString "&lt;" <> LazyText.fromStrict n <> fromString "&gt;" <>
          fromString "</a>"
        Literal l ->
          fromString "'" <> LazyText.fromStrict l <> fromString "'"
        Or values' ->
          LazyText.intercalate (fromString " | ") (renderProductionValue <$> toList values')
        Sequence values' ->
          case values' of
            value' :| [] -> renderProductionValue value'
            _ -> LazyText.intercalate (fromString " ") (renderSequenceValue <$> toList values')
          where
            renderSequenceValue v@Or{} = parens $ renderProductionValue v
            renderSequenceValue v = renderProductionValue v
        Class classes ->
          fromString "[" <> foldMap renderClass classes <> fromString "]"
        NotClass classes ->
          fromString "[^" <> foldMap renderClass classes <> fromString "]"
        Optional value' ->
          renderOptionalValue value' <> fromString "?"
          where
            renderOptionalValue v@Sequence{} = parens $ renderProductionValue v
            renderOptionalValue v@Or{} = parens $ renderProductionValue v
            renderOptionalValue v = renderProductionValue v
        Many value' ->
          renderManyValue value' <> fromString "*"
          where
            renderManyValue v@Sequence{} = parens $ renderProductionValue v
            renderManyValue v@Or{} = parens $ renderProductionValue v
            renderManyValue v = renderProductionValue v
        Some value' ->
          renderSomeValue value' <> fromString "+"
          where
            renderSomeValue v@Sequence{} = parens $ renderProductionValue v
            renderSomeValue v@Or{} = parens $ renderProductionValue v
            renderSomeValue v = renderProductionValue v

main :: IO ()
main = do
  cli <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)
  case cli of
    CliVersion version ->
      case lookup version versions of
        Nothing -> do
          hPutStrLn stderr $ "error: unknown version " ++ show cli.version
          hPutStrLn stderr "available versions:"
          for_ versions $  Text.hPutStrLn stderr . fst
        Just (SomeCodec codec) -> do
          let (defs, _ty) = runDocumented $ documentedDef codec
          for_ defs $ LazyText.putStrLn . Pretty.render . docDocumented
    CliGrammar ->
      LazyText.putStrLn $ foldMap renderProduction grammar
