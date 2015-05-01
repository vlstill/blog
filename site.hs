--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Data.Maybe
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

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
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
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["atom.xml"] $ feedRule renderAtom
    create ["rss.xml"]  $ feedRule renderRss

    match "templates/*" $ compile templateCompiler


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
