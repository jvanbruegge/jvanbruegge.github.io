module Main (main) where

import Data.Aeson (FromJSON, ToJSON (..), Value (..), object, (.=))
import Data.Aeson.Optics (key, members, values, _Object, _String)
import Data.Binary (Binary)
import Data.Functor (void)
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, pack, replace, splitOn, strip, unpack)
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Development.Shake (Action, ShakeOptions (..), Verbosity (..), copyFileChanged, forP, getDirectoryFiles, liftIO, parallel, readFile', shakeOptions, writeFile')
import Development.Shake.FilePath (dropExtension, (</>))
import Development.Shake.Forward (forwardOptions, shakeArgsForward)
import GHC.Generics (Generic)
import Optics (over, (%), (^?))
import Slick (compileTemplate', convert, substitute)
import Slick.Pandoc (PandocReader, PandocWriter, defaultHtml5Options, defaultMarkdownOptions, makePandocReaderWithMetaWriter)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Export as Export
import Text.Feed.Types (Feed (AtomFeed))
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
    category :: String,
    description :: String,
    date :: String,
    url :: String,
    tags :: [Tag]
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON, Binary)

data PostList = MkPostList
  { posts :: [Post],
    heading :: String
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
  (_ : cat : _) <- pure . splitOn "/" . pack $ srcPath

  let setCategory = over _Object (HM.insert "category" (String cat))
      setTags =
        over
          (key "tags" % values)
          ( \t ->
              object ["name" .= over _String strip t, "html" .= fromJust (lookup (strip . fromJust $ t ^? _String) tagList)]
          )
      postUrl = dropExtension srcPath </> "index.html"
      setPostUrl = over _Object (HM.insert "url" (String $ "/" <> pack postUrl))
      fixNewlines = over (members % _String) (replace "\n" " ")
      fullData = setTags . setCategory . setPostUrl . fixNewlines $ postData

  template <- compileTemplate' "template/post.html"
  writeFile' (outputFolder </> postUrl) . unpack $ substitute template fullData

  convert fullData

buildPostsLists :: [Post] -> Action [PostList]
buildPostsLists postList = parallel $
  flip map ["programming", "cooking", "reviews", "all"] $ \cat -> do
    liftIO . putStrLn $ "Building post list for category " <> cat
    let p = filter (\x -> cat == "all" || category x == cat) postList
        h = if cat == "all" then "All posts" else "All posts in '" <> cat <> "'"
        result = MkPostList p h
        file = if cat == "all" then "index.html" else cat </> "index.html"

    template <- compileTemplate' "template/postList.html"
    writeFile' (outputFolder </> file) . unpack $ substitute template (toJSON result)

    pure result

buildTagLists :: [(Text, Text)] -> [Post] -> Action [PostList]
buildTagLists tagList postList = parallel $
  flip map (map fst tagList) $ \tag -> do
    liftIO . putStrLn $ "Building post list for tag " <> unpack tag
    let p = filter (\x -> unpack tag `elem` map name (tags x)) postList
        h = "All posts in tag '" <> unpack tag <> "'"
        result = MkPostList p h

    template <- compileTemplate' "template/postList.html"
    writeFile' (outputFolder </> "tags" </> unpack tag </> "index.html") . unpack $ substitute template (toJSON result)

    pure result

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

buildPosts :: [(Text, Text)] -> Action [Post]
buildPosts tagList = do
  tmpl <- loadPandocTemplate
  paths <- getDirectoryFiles "articles" ["//*.md"]
  forP paths (buildPost tmpl tagList)

buildAtomFeed :: [Post] -> Action ()
buildAtomFeed posts = do
  currentDate <- liftIO getCurrentTime
  let feed =
        ( Atom.nullFeed
            "https://jvanbruegge.github.io/atom.xml"
            (Atom.TextString "jvanbruegge's blog")
            (pack $ iso8601Show currentDate)
        )
          { Atom.feedEntries = fmap postToEntry posts
          }
  Just renderedFeed <- pure $ Export.textFeedWith def $ AtomFeed feed
  writeFile' (outputFolder </> "atom.xml") $ LT.unpack renderedFeed
  where
    postToEntry MkPost {url, title, date, author, description} =
      let url' = "https://jvanbruegge.github.io" <> url
       in (Atom.nullEntry (pack url') (Atom.TextString $ pack title) (pack date))
            { Atom.entryAuthors = [Atom.nullPerson {Atom.personName = pack author}],
              Atom.entryLinks = [Atom.nullLink $ pack url'],
              Atom.entryContent = Just (Atom.TextContent $ pack description),
              Atom.entryPublished = Just $ pack date
            }

copyStaticFiles :: Action ()
copyStaticFiles = do
  paths <- getDirectoryFiles "./template" ["images//*", "//*.css", "impressum.html"]
  void $
    forP paths $ \p ->
      copyFileChanged ("template" </> p) (outputFolder </> p)

buildRules :: Action ()
buildRules = do
  tagList <- loadTags
  postList <- sortBy (\a b -> compare (date b) (date a)) <$> buildPosts tagList
  _ <- buildPostsLists postList
  _ <- buildTagLists tagList (take 15 postList)
  buildAtomFeed postList
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
