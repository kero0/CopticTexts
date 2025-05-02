{-# LANGUAGE OverloadedStrings #-}

module Pandoc where

import Data.List (sort, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, splitOn, stripSuffix, toLower)
import Parser hiding (words)
import Text.Pandoc.Builder qualified as P

toPandoc :: Chapter -> P.Pandoc
toPandoc (Chapter (Header bookname _ chapternumber) verses) =
    P.setTitle
        ( P.text
            ( bookname
                <> " Chapter "
                <> tshow chapternumber
            )
        )
        $ P.doc
        $ flatten
        $ P.fromList
        $ map verseToBlock verses

mkIndex :: FilePath -> [FilePath] -> P.Pandoc
mkIndex parent files =
    P.setTitle "Index" $
        P.doc $
            P.bulletList $
                map (P.plain . mkLink . pack . mStripPrefix parent) $
                    sort files

mStripPrefix :: [Char] -> [Char] -> [Char]
mStripPrefix parent f = maybe f stripLeadingSlash $ stripPrefix parent f
  where
    stripLeadingSlash ('/' : xs) = stripLeadingSlash xs
    stripLeadingSlash xs = xs

mkLink :: Text -> P.Inlines
mkLink path =
    let noSuffix = (\s -> fromMaybe s $ stripSuffix ".org" s) path
     in P.link
            ((<> "/") noSuffix)
            noSuffix
            (P.text noSuffix)

baseName :: Text -> Text
baseName =
    toLower
        . last
        . splitOn "/"

verseToBlock :: Verse -> P.Blocks
verseToBlock (Verse (VerseReference _ versenumber) text_en text words') =
    P.header 1 (P.text (tshow versenumber) <> " " <> P.text text)
        <> P.para (P.text text_en)
        <> flatten
            ( P.fromList $
                map (wordToBlock 2) words'
            )
wordToBlock :: Int -> Parser.Word -> P.Blocks
wordToBlock depth (Parser.WordRange rangeStart rangeEnd form misc subwords) =
    P.header depth (P.text $ tshow rangeStart <> "-" <> tshow rangeEnd <> " " <> form)
        <> P.para (P.text $ if misc == "_" then "" else misc)
        <> flatten
            ( P.fromList $
                map (wordToBlock (depth + 1)) subwords
            )
wordToBlock depth (Parser.Word idx form lemma universal_part_of_speech language_specific_part_of_speech morphological_features word_head universal_dependency_relation misc) =
    P.header depth (P.text $ tshow idx <> ". " <> form)
        <> P.simpleTable
            (map (P.plain . P.text) ["Field", "Value"])
            ( map
                (map $ P.plain . P.text)
                [ ["Lemma", lemma]
                , ["UPOS", universal_part_of_speech]
                , ["XPOS", language_specific_part_of_speech]
                , ["FEATS", morphological_features]
                , ["HEAD", tshow word_head]
                , ["DEPREL", universal_dependency_relation]
                , ["MISC", misc]
                ]
            )

replaceEmpty :: Text -> Text
replaceEmpty s = if s == "" || s == "_" then "N/A" else s

flatten :: P.Many (P.Many a) -> P.Many a
flatten = P.fromList . mconcat . map P.toList . P.toList

tshow :: (Show a) => a -> Text
tshow = pack . show
