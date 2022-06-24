{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec.Char
import qualified Data.List as List
import Text.ParserCombinators.ReadP (skipSpaces)
import Data.Maybe (fromMaybe, maybeToList)

type Parser = Parsec Void Text

type Name = Text
type Version = [Text]

data Constraint
    = Wanted Name
    | LowerThan Name Version
    | GreaterEqualThan Name Version
    | CompatibleWith Name Version
    | OrConstraint Constraint Constraint

constraintRule :: Constraint -> Text
constraintRule (Wanted name) = "wanted(" <> slugifyName name <> ")."
constraintRule (LowerThan name v) = "lt(" <> slugifyName name <> "," <> prettyVersion v <> ")."
constraintRule (GreaterEqualThan name v) = "get(" <> slugifyName name <> "," <> prettyVersion v <> ")."
constraintRule (CompatibleWith name v) = "compW(" <> slugifyName name <> "," <> prettyVersion v <> ")."
constraintRule (OrConstraint c1 c2) = constraintRule c1 <> " | " <> constraintRule c2

prettyVersion :: Version -> Text
prettyVersion xs = "(" <> T.intercalate ", " xs <> ")"

slugifyName :: Name -> Name
slugifyName name = T.replace "-" "_" name

parser :: Text -> Either (ParseErrorBundle Text Void) [Constraint]
parser input = runParser parseConstraints "" input

parseConstraints :: Parser [Constraint]
parseConstraints = do
    skipManyTill anySingle (string "Dependencies loaded: ")
    constraints <- sepBy1 parseConstraint (string "," >> space)
    pure $ List.concat constraints

parseConstraint :: Parser [Constraint]
parseConstraint = do
    name <- manyTill anySingle spaceChar
    space
    constraint <- try (disjParser $ T.pack name) <|> conjParser (T.pack name)
    pure $ Wanted (T.pack name) : constraint

conjParser :: Text ->  Parser [Constraint]
conjParser name = do
    c1 <- choice [greaterEqConstraintParser name, smallerConstraintParser name, greaterCompConstraintParser name]
    mc2 <- optional . try $ do
        space
        string "&&"
        space
        choice [greaterEqConstraintParser name, smallerConstraintParser name, greaterCompConstraintParser name]
    pure $ c1 : maybeToList mc2

disjParser :: Text -> Parser [Constraint]
disjParser name = do
    c1 <- choice [greaterEqConstraintParser name, smallerConstraintParser name, greaterCompConstraintParser name]
    space
    string "||"
    space
    c2 <- choice [greaterEqConstraintParser name, smallerConstraintParser name, greaterCompConstraintParser name]
    pure [OrConstraint c1 c2]


greaterCompConstraintParser :: Text -> Parser Constraint
greaterCompConstraintParser name = do
    string "^>="
    versionNums <- versionParser
    pure $ CompatibleWith name versionNums

greaterEqConstraintParser :: Text -> Parser Constraint
greaterEqConstraintParser name = do
    string ">="
    versionNums <- versionParser
    pure $ GreaterEqualThan name versionNums

smallerConstraintParser :: Text -> Parser Constraint
smallerConstraintParser name = do
    string "<"
    versionNums <- versionParser
    pure $ LowerThan name versionNums

versionParser :: Parser Version
versionParser = do
    a <- some digitChar
    optional $ char '.'
    b <- optional $ some digitChar
    optional $ char '.'
    c <- optional $ some digitChar
    optional $ char '.'
    d <- optional $ some digitChar
    let versionNums = map T.pack
            [ a
            , fromMaybe "-1" b
            , fromMaybe "-1" c
            , fromMaybe "-1" d
            ]

    pure versionNums
