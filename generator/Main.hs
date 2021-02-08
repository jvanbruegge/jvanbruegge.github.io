module Main (main) where

import Data.Aeson (FromJSON, ToJSON, Value (..), object, (.=))
import Data.Aeson.Optics (key, values, _String)
import Data.Binary (Binary)
import Data.Functor (void)
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, pack, splitOn, strip, unpack)
import Development.Shake (Action, ShakeOptions (..), Verbosity (..), copyFileChanged, forP, getDirectoryFiles, liftIO, readFile', shakeOptions, writeFile')
import Development.Shake.FilePath (dropExtension, (</>))
import Development.Shake.Forward (forwardOptions, shakeArgsForward)
import GHC.Generics (Generic)
import Optics (over, (%), (^?))
import Slick (compileTemplate', convert, substitute)
import Slick.Pandoc (PandocReader, PandocWriter, defaultHtml5Options, defaultMarkdownOptions, makePandocReaderWithMetaWriter)
import Text.Pandoc.Class (PandocIO, runIO, setVerbosity)
import Text.Pandoc.Logging (Verbosity (..))
import Text.Pandoc.Options (WriterOptions (..), def)
import Text.Pandoc.Readers (readMarkdown)
import Text.Pandoc.Templates (Template, compileTemplate)
import Text.Pandoc.Writers (writeHtml5String, writePlain)

outputFolder :: FilePath
outputFolder = "docs/"

data Tag = MkTag
  { name :: String,
    html :: String
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON, Binary)

data Post = MkPost
  { title :: String,
    author :: String,
    content :: String,
    date :: String,
    tags :: [Tag]
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON, Binary)

markdownToHTML :: Template Text -> Text -> Action Value
markdownToHTML tmpl =
  loadUsing
    (readMarkdown defaultMarkdownOptions)
    (\doc -> setVerbosity ERROR *> writeHtml5String html5Opts doc)
    (Just $ writePlain def)
  where
    html5Opts =
      defaultHtml5Options
        { writerTableOfContents = True,
          writerTOCDepth = 4,
          writerTemplate = Just tmpl
        }

buildPost :: Template Text -> [(Text, Text)] -> FilePath -> Action Post
buildPost tmpl tagList srcPath = do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postMarkdown <- readFile' $ "articles" </> srcPath
  postData <- markdownToHTML tmpl . pack $ postMarkdown
  (year : _) <- pure . splitOn "/" . pack $ srcPath

  let setYear = over (key "date" % _String) (<> ", " <> year)
      setTags =
        over
          (key "tags" % values)
          ( \t ->
              object ["name" .= t, "html" .= fromJust (lookup (strip . fromJust $ t ^? _String) tagList)]
          )
      fullData = setYear . setTags $ postData

  template <- compileTemplate' "template/index.html"
  writeFile' (outputFolder </> dropExtension srcPath </> "index.html") . unpack $ substitute template fullData

  convert fullData

loadPandocTemplate :: Action (Template Text)
loadPandocTemplate = do
  template <- pack <$> readFile' "template/article.html"
  Right x <- pure . runIdentity $ compileTemplate "" template
  pure x

loadTags :: Action [(Text, Text)]
loadTags = do
  paths <- getDirectoryFiles "template/tags" ["*.html"]
  forP paths $ \p -> do
    h <- readFile' $ "template/tags" </> p
    pure (pack (dropExtension p), pack h)

buildPosts :: Action [Post]
buildPosts = do
  tmpl <- loadPandocTemplate
  paths <- getDirectoryFiles "articles" ["//*.md"]
  tagList <- loadTags
  forP paths (buildPost tmpl tagList)

copyStaticFiles :: Action ()
copyStaticFiles = do
  paths <- getDirectoryFiles "./template" ["images//*", "//*.css", "impressum.html"]
  void $
    forP paths $ \p ->
      copyFileChanged ("template" </> p) (outputFolder </> p)

buildRules :: Action ()
buildRules = do
  _ <- buildPosts
  copyStaticFiles

main :: IO ()
main = shakeArgsForward shOpts buildRules
  where
    shOpts = forwardOptions $ shakeOptions {shakeVerbosity = Verbose, shakeLintInside = ["articles", "template"]}

loadUsing :: PandocReader textType -> PandocWriter -> Maybe PandocWriter -> textType -> Action Value
loadUsing reader writer metaWriter text = do
  (pdoc, meta) <- makePandocReaderWithMetaWriter reader (fromMaybe writer metaWriter) text
  outText <- unPandocM $ writer pdoc
  case meta of
    Object m -> return . Object $ HM.insert "content" (String outText) m
    -- meta & _Object . at "content" ?~ String outText
    _ -> fail "Failed to parse metadata"

unPandocM :: PandocIO a -> Action a
unPandocM p = do
  result <- liftIO $ runIO p
  either (fail . show) return result
