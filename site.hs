--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings
           , DeriveDataTypeable
           , DeriveGeneric
           #-}

import           Data.Monoid
import           Data.Maybe
import           Data.Ord
import           Data.Char
import qualified Data.HashMap.Lazy as M
import           Data.List
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Time.LocalTime

import           Data.Typeable ( Typeable )
import           Data.Data ( Data )
import           GHC.Generics ( Generic )
import           Data.Binary
import           Data.ByteString.Lazy ( ByteString )

import           System.IO.Unsafe ( unsafePerformIO )
import           System.FilePath
import           System.Process ( system )

import           Control.Monad
import           Control.Applicative
import           Control.Arrow

import           Text.Pandoc.Options

import           Hakyll


--------------------------------------------------------------------------------

dbg :: Show a => a -> a
dbg x = unsafePerformIO (print x) `seq` x

main :: IO ()
main = hakyll $ do
    match "images/*/meta" $ compile imgMetaCompiler

    match "images/*/*.*" $ do
        route   idRoute
        compile $ getResourceLBS
            >>= withItemBody (unixFilterLBS "convert" [ "-", "-strip", "-resize", "3000x3000", "-quality", "94", "-" ])

    match "images/*/*.*" $ version "small" $ do
        route $ mapRoute (splitExtension >>> \(name, ext) -> (name ++ "-small") <.> ext)
        compile $ getResourceLBS
              >>= withItemBody (unixFilterLBS "convert" [ "-", "-strip", "-thumbnail", "250x250", "-quality", "96", "-" ])

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "script/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fancybox/*" $ do
        route   idRoute
        compile copyFileCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let postCtxBase = postCtxBase' tags

    tagsRules tags $ \tag pattern -> do
        let title = "Téma: " ++ tag ++ ""
        route (customRoute $ ident . toFilePath) -- idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtxBase (return posts)
                      `mappend` defContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["tags.html"] $ do
        route idRoute
        let ctx = constField "title" "Témata" <> defContext
        compile $ renderTags showTag concat (sortTagsBy (down $ comparing (length . snd)) tags)
            >>= makeItem . (\str -> "<ul>" ++ str ++ "</ul>")
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    forM_ ["posts/*", "extras/*"] $ \x -> match x $ do
        route $ setExtension "html"
        compile $ do
            path <- toFilePath <$> getUnderlying
            let
                basename = dropExtension $ takeFileName path
                imgbase = "images" </> basename
                imgpat = fromGlob . (</> "*.*") $ imgbase
                metapat = fromList . (: []) . fromFilePath . (</> "meta") $ imgbase
            imgs <- sortBy (comparing (toFilePath . itemIdentifier)) <$> loadAll (imgpat .&&. hasNoVersion)
            unsafeCompiler $ system (unwords ["cd _site/images && zip -r ", addExtension basename "zip", basename, "-x '*-small.*'"])

            meta <- fromMaybe [] . fmap itemBody . head' <$> loadAllSnapshots metapat "metamap"

            let ctx = postCtx tags imgs meta
            pandocTemplateCompiler ctx
                >>= loadAndApplyTemplate "templates/post.html"    ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "blog.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogCtx =
                    listField "posts" postCtxBase (return posts) `mappend`
                    defContext

            pandocTemplateCompiler blogCtx
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls


    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- fmap (take 8) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtxBase (return posts) <>
                    constField "title" "Home"                <>
                    constField "hidetitle" "hile"            <>
                    defContext

            pandocTemplateCompiler indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["atom.xml"] $ feedRule tags renderAtom
    create ["rss.xml"]  $ feedRule tags renderRss

    match "templates/*" $ compile templateCompiler

  where
    showTag tag url count _ _ = "<li><a href=\"" ++ url ++ "\">" ++ tag ++
                                  " (" ++ show count ++ ")</a></li>"

--------------------------------------------------------------------------------
feedRule :: Tags -> (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)) -> Rules ()
feedRule tags render = do
    route idRoute
    compile $ do
        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
        render feedConfig (feedCtx tags) posts

feedCtx :: Tags -> Context String
feedCtx tags = desc <> postCtxBase' tags
  where
    desc = field "description" $ \item -> do
        metadata <- getMetadata (itemIdentifier item)
        return . fromMaybe "" $ mToStr =<< M.lookup "short" metadata

mToStr :: A.Value -> Maybe String
mToStr (A.String x) = Just (T.unpack x)
mToStr _ = Nothing

postCtxBase' :: Tags -> Context String
postCtxBase' tags =
    dateField "date" "%-d. %-m. %Y" <>
    tagsField "tags" tags <>
    field "firstSentence" extractFirstSentence <>
    defContext
  where
    extractFirstSentence :: Item String -> Compiler String
    extractFirstSentence item = do
        meta <- getMetadata (itemIdentifier item)
        return $ maybe "" (takeWhile (/= '.') >>> (++ "…")) $ mToStr =<< M.lookup "short" meta

imgToZip :: Identifier -> FilePath
imgToZip = flip addExtension "zip" . takeDirectory . toFilePath

postCtx :: Tags -> [Item ByteString] -> [ImgMeta] -> Context String
postCtx tags imgs imgmeta = if null imgs then base
                                         else constField "imageszip" imageszip <> base
  where
    base = listField "images" imgCtx (return imgs) <>
           postCtxBase' tags
    imageszip :: FilePath
    imageszip = (".." </>) . imgToZip . itemIdentifier $ head imgs
    imgCtx = urlField "url" <>
             field "smallUrl" getSmallUrl <>
             field "alt" getAlt
    getSmallUrl item = let id = setVersion (Just "small") (itemIdentifier item)
                       in toUrl . fromJust <$> getRoute id
    findMeta item = let  name = takeFileName . toFilePath $ itemIdentifier item
                    in findMeta' imgmeta name
    findMeta' [] name = ImgMeta { imgPattern = name, imgTags = [], imgAlt = name }
    findMeta' (m:ms) name
        | fromGlob (imgPattern m) `matches` fromFilePath name = m
        | otherwise = findMeta' ms name
    getAlt item = return $ imgAlt (findMeta item)

defContext :: Context String
defContext = constField "years" years <> defaultContext
  where
    (year, _, _) = toGregorian $ localDay timestamp
    years = if year == firstyear then show firstyear
                                 else show firstyear ++ " – " ++ show year
    firstyear = 2015

-- | compile pandoc, but apply it (on itself) as template first
pandocTemplateCompiler :: Context String -> Compiler (Item String)
pandocTemplateCompiler ctx = getResourceBody >>= applyAsTemplate ctx >>= render
  where
    render = renderPandocWith defaultHakyllReaderOptions
        (defaultHakyllWriterOptions { writerHtml5 = True })

data ImgMeta = ImgMeta { imgPattern :: String
                       , imgAlt :: String
                       , imgTags :: [String]
                       }
             deriving ( Eq, Show, Read, Data, Typeable, Generic )

instance Binary ImgMeta -- derived using Generic

imgMetaCompiler :: Compiler (Item String)
imgMetaCompiler = fmap imgMap <$> getResourceBody
    >>= saveSnapshot "metamap"
    >>= return . fmap show
  where
    imgMap :: String -> [ImgMeta]
    imgMap = lines >>> map imgMeta
    imgMeta :: String -> ImgMeta
    imgMeta x = let (pat, ':':rest) = span (/= ':') x
                    (alt, ';':tags) = span (/= ';') rest
                in ImgMeta { imgPattern = trim pat
                           , imgAlt = trim alt
                           , imgTags = splitAll "( |\t)*,( |\t)" (trim tags)
                           }

mapRoute :: (FilePath -> FilePath) -> Routes
mapRoute f = customRoute (f . toFilePath)

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Vladimír Štill: blog"
    , feedDescription = "blog"
    , feedAuthorName  = "Vladimír Štill"
    , feedAuthorEmail = "vl.still@gmail.com"
    , feedRoot        = "https://paradise.fi.muni.cz/~xstill"
    }

ident :: String -> String
ident = map (\x -> fromMaybe x (x `M.lookup` tr)) . map toLower
  where
    tr = M.fromList
        [ (' ', '-'), ('\n', '-'), ('*', '-'), ('\t', '-')
        , ('ě', 'e'), ('š', 's'), ('č', 'c'), ('ř', 'r'), ('ž', 'z')
        , ('ý', 'y'), ('á', 'a'), ('í', 'i'), ('é', 'e'), ('ú', 'u')
        , ('ů', 'u'), ('ó', 'o')
        , (',', '-'), (';', '-')
        ]

down :: (a -> a -> Ordering) -> a -> a -> Ordering
down cmp a b = case cmp a b of
                  EQ -> EQ
                  LT -> GT
                  GT -> LT

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

timestamp :: LocalTime
timestamp = unsafePerformIO $ do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    return $ utcToLocalTime timezone now

debug :: Show a => a -> a
debug x = unsafePerformIO (print x) `seq` x
