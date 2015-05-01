--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Data.Maybe
import           Data.Ord
import qualified Data.Map as M
import           Control.Applicative
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Téma: " ++ tag ++ ""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["tags.html"] $ do
        route idRoute
        let ctx = constField "title" "Témata" <> defaultContext
        compile $ renderTags showTag concat (sortTagsBy (down $ comparing (length . snd)) tags)
            >>= makeItem . (\str -> "<ul>" ++ str ++ "</ul>")
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        let ctx = postCtxWithTags tags
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog"                `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 8) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    constField "hidetitle" "hile"            <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["atom.xml"] $ feedRule renderAtom
    create ["rss.xml"]  $ feedRule renderRss

    match "templates/*" $ compile templateCompiler

  where
    showTag tag url count _ _ = "<li><a href=\"" ++ url ++ "\">" ++ tag ++
                                  " (" ++ show count ++ ")</a></li>"

--------------------------------------------------------------------------------
feedRule :: (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)) -> Rules ()
feedRule render = do
    route idRoute
    compile $ do
        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
        render feedConfig feedCtx posts

postCtx :: Context String
postCtx =
    dateField "date" "%-d. %-m. %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

feedCtx :: Context String
feedCtx = postCtx <> (field "description" $ \item -> do
            metadata <- getMetadata (itemIdentifier item)
            return $ fromMaybe "" $ M.lookup "short" metadata)
    

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Vladimír Štill: blog"
    , feedDescription = "blog"
    , feedAuthorName  = "Vladimír Štill"
    , feedAuthorEmail = "vl.still@gmail.com"
    , feedRoot        = "https://paradise.fi.muni.cz/~xstill/blog.html"
    }

down :: (a -> a -> Ordering) -> a -> a -> Ordering
down cmp a b = case cmp a b of
                  EQ -> EQ
                  LT -> GT
                  GT -> LT       
