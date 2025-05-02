{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((<=<))
import Data.Text hiding (length, take)
import Data.Text.IO qualified as TIO
import Pandoc (mkIndex, toPandoc)
import Parser (chapterP)
import System.Environment (getArgs)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Pandoc (Pandoc, PandocIO, Template, WriterOptions (..), compileTemplate, getDefaultTemplate, glob, handleError, runIO, writeOrg)
import Text.Pandoc.Options (def)
import UnliftIO.Async (pooledMapConcurrently)

func :: WriterOptions -> Pandoc -> PandocIO Text
func = writeOrg
ext :: [Char]
ext = "org"

main :: IO ()
main =
    getArgs
        >>= mapM (handleError <=< runIO . (\p -> (p,) <$> glob (p <> "/**/*.conllu")))
        >>= mapM_ (uncurry handleDir)

handleDir :: FilePath -> [FilePath] -> IO ()
handleDir parent files = do
    template <- (fmap (\(Right x) -> Just x) . compileTemplate "" <=< handleError <=< runIO) $ getDefaultTemplate (pack ext)
    outputs <- pooledMapConcurrently (convert template) files
    runIO (func ((\x -> x{writerTableOfContents = False}) (options template)) $ mkIndex parent outputs)
        >>= handleError
        >>= TIO.writeFile (parent <> "/_index." <> ext)
        >> print ("Completed: " <> parent <> "/_index." <> ext)

options :: Maybe (Template Text) -> WriterOptions
options template =
    def
        { writerTableOfContents = True
        , writerTOCDepth = 2
        , writerSectionDivs = True
        , writerTemplate = template
        }

addHeader :: Text -> Text
addHeader = (<>) ""

convert :: Maybe (Template Text) -> FilePath -> IO String
convert template file = do
    text <- TIO.readFile file
    let outputPath = take (length file - 6) file <> ext
    case parse chapterP file text of
        Left output -> print (errorBundlePretty output) >> print ("Couldn't parse input file " <> file)
        Right x ->
            runIO (func (options template) $ toPandoc x)
                >>= handleError
                >>= TIO.writeFile outputPath . addHeader
    print $ "Completed: " <> file
    return outputPath
