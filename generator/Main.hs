module Main (main) where

import Data.Aeson (FromJSON, ToJSON, Value(..), object, (.=))
import Data.Aeson.Optics (values, key, _String)
import Data.Maybe (fromJust)
import Data.Binary (Binary)
import Data.Functor (void)
import Data.Text (Text, pack, splitOn, unpack)
import Development.Shake (writeFile', Action, ShakeOptions (..), Verbosity (..), copyFileChanged, forP, getDirectoryFiles, liftIO, readFile', shakeOptions)
import Development.Shake.FilePath (dropExtension, (</>))
import Development.Shake.Forward (forwardOptions, shakeArgsForward)
import GHC.Generics (Generic)
import Optics (over, (%), (^?))
import Slick (compileTemplate', substitute, convert, markdownToHTML)

outputFolder :: FilePath
outputFolder = "docs/"

data Tag = MkTag
    { name :: String
    , html :: String }
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

buildPost :: [(Text, Text)] -> FilePath -> Action Post
buildPost tagList srcPath = do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postMarkdown <- readFile' $ "articles" </> srcPath
  postData <- markdownToHTML . pack $ postMarkdown
  (year : _) <- pure . splitOn "/" . pack $ srcPath

  let setYear = over (key "date" % _String) (<> ", " <> year)
      setTags = over (key @Value "tags" % values) (\t ->
        object [ "name" .= t, "html" .= fromJust (lookup (fromJust $ t ^? _String) tagList)])
      fullData = setYear . setTags $ postData

  template <- compileTemplate' "template/index.html"
  writeFile' (outputFolder </> dropExtension srcPath </> "index.html") . unpack $ substitute template fullData

  convert fullData

loadTags :: Action [(Text, Text)]
loadTags = do
  paths <- getDirectoryFiles "template/tags" ["*.html"]
  forP paths $ \p -> do
        h <- readFile' $ "template/tags" </> p
        pure (pack (dropExtension p), pack h)

buildPosts :: Action [Post]
buildPosts = do
  paths <- getDirectoryFiles "articles" ["//*.md"]
  tagList <- loadTags
  forP paths (buildPost tagList)

copyStaticFiles :: Action ()
copyStaticFiles = do
  paths <- getDirectoryFiles "./template" ["images//*", "//*.css"]
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
