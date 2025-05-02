{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative ((<|>))
import Control.Monad (mzero, unless)
import Data.Char (isDigit, isPrint, isSpace)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, count, eof, lookAhead, sepEndBy, some, takeWhile1P, try)
import Text.Megaparsec.Char
import Text.Read (readMaybe)
import Prelude hiding (Word, words)

data VerseReference = VerseReference
    { header :: Header
    , versenumber :: Int
    }
    deriving (Show)

data Header = Header
    { bookname :: T.Text
    , booknumber :: Int
    , chapternumber :: Int
    }
    deriving (Show)

data Word
    = Word
        { idx :: Int
        , form :: T.Text
        , lemma :: T.Text
        , universal_part_of_speech :: T.Text -- see https://universaldependencies.org/u/pos/index.html
        , language_specific_part_of_speech :: T.Text
        , morphological_features :: T.Text
        , word_head :: Int
        , universal_dependency_relation :: T.Text
        , -- , dependency_graph :: [(Int, T.Text)] -- not used
          misc :: T.Text
        }
    | WordRange
        { rangeStart :: Int
        , rangeEnd :: Int
        , form :: T.Text
        , misc :: T.Text
        , subwords :: [Word]
        }
    deriving (Show)

data Verse
    = Verse
    { sent_id :: VerseReference
    , text_en :: T.Text
    , text :: T.Text
    , words :: [Word]
    }
    deriving (Show)

data Chapter = Chapter
    { bookInfo :: Header
    , verses :: [Verse]
    }
    deriving (Show)

type Parser = Parsec Void T.Text

chapterP :: Parser Chapter
chapterP =
    chapterPMain <|> (Chapter (Header "empty book" 0 0) [] <$ takeWhileP Nothing isSpace <* eof)
  where
    chapterPMain = do
        bookInfo <- headerP
        verses <- try verseP `sepEndBy` space
        eof
        return
            Chapter
                { bookInfo
                , verses
                }

wordP :: Parser Word
wordP = do
    try wordRangeP <|> try singleWordP

tsvElementP :: Parser T.Text
tsvElementP = takeWhileP Nothing (\x -> x /= '\t' && x /= '\n') <* (char '\t' <|> return '\t')

singleWordP :: Parser Word
singleWordP = do
    idx <- safeRead =<< tsvElementP
    form <- tsvElementP
    lemma <- tsvElementP
    universal_part_of_speech <- tsvElementP
    language_specific_part_of_speech <- tsvElementP
    morphological_features <- tsvElementP
    word_head <- safeRead =<< tsvElementP
    universal_dependency_relation <- tsvElementP
    _dependency_graph <- [] <$ tsvElementP
    misc <- tsvElementP <* eol
    return $
        Word
            { idx
            , form
            , lemma
            , universal_part_of_speech
            , language_specific_part_of_speech
            , morphological_features
            , word_head
            , universal_dependency_relation
            , misc
            }

wordRangeP :: Parser Word
wordRangeP = do
    (rangeStart, rangeEnd) <-
        ((,) <$> numP)
            <*> (char '-' *> numP <* char '\t')
    form <- tsvElementP
    _ <- count 7 $ char '_' >> char '\t'
    misc <- strP <* eol
    subwords <- some $ try (subwordP rangeEnd)
    return
        WordRange
            { rangeStart
            , rangeEnd
            , form
            , misc
            , subwords
            }
  where
    subwordP :: Int -> Parser Word
    subwordP rangeEnd = do
        lookAhead $ do
            idx <- numP <* char '\t'
            unless (idx <= rangeEnd) mzero
        singleWordP

verseP :: Parser Verse
verseP = do
    sent_id <- sentIdP
    (text_en, text) <- try textP1 <|> textP2
    words <- some $ try wordP
    return
        Verse
            { sent_id
            , text_en
            , text
            , words
            }
  where
    textP = ((string "# text = " *> strP) <|> ("" <$ string "# text")) <* eol
    textEnP = ((string "# text_en = " *> strP) <|> ("" <$ string "# text_en")) <* eol
    textP1 = do
        text_en <- textEnP
        text <- textP
        return (text_en, text)
    textP2 = do
        text <- textP
        text_en <- textEnP
        return (text_en, text)

sentIdP :: Parser VerseReference
sentIdP = try sentIdP1 <|> try sentIdP2
  where
    sentIdP1 = do
        header <- string "# sent_id = " *> takeWhile1P Nothing (/= '-') *> char '-' *> headerP'
        versenumber <- char '_' *> char 's' *> numP <* eol
        return
            VerseReference
                { header
                , versenumber
                }
    sentIdP2 = do
        chars <- string "# sent_id = " *> takeWhile1P Nothing (/= '-') *> char '-' *> strP <* eol
        if T.take 2 (T.drop (T.length chars - 6) chars) /= "_s"
            then mzero
            else
                let bookname = T.take (T.length chars - 6) chars
                    booknumber = 0
                    chapternumber = 0
                    versenumber' = safeRead $ T.drop (T.length chars - 4) chars
                    header = Header{bookname, booknumber, chapternumber}
                 in versenumber' >>= \versenumber -> return VerseReference{header, versenumber}

headerP :: Parser Header
headerP = string "# newdoc id = " *> takeWhileP Nothing (/= ':') *> char ':' *> headerP' <* eol

headerP' :: Parser Header
headerP' = do
    booknumber <- try (numP <* char '_') <|> pure 0
    bookname <- strP
    chapternumber <- char '_' *> numP <|> pure 1
    return
        Header
            { bookname
            , booknumber
            , chapternumber
            }

numP :: Parser Int
numP = takeWhile1P (Just "number") isDigit >>= safeRead
strP :: Parser T.Text
strP = takeWhile1P (Just "printable") isPrint

safeRead :: T.Text -> Parser Int
safeRead = maybe mzero return . readMaybe . T.unpack
