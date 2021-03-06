--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import qualified Data.Set as S
import           Text.Pandoc
import           Hakyll.Core.Compiler

import qualified CssVars as CV
import           CustomCompilers
import           Collections

import qualified Data.Map as M
import Data.Maybe


--------------------------------------------------------------------------------


main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile $ compressCssCompiler >>= applyAsTemplate CV.defaultCtx

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/elm/elm.js" $ do
        route   idRoute
        compile copyFileCompiler

    match "where.html" $ do
        route   idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate headContext
                >>= loadAndApplyTemplate "templates/default.html" headContext
                >>= relativizeUrls

    match "font/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["cv.pdf", "CNAME"]) $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.md", "projects.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    colls <- buildCollections "drafts/*" (fromCapture "collections/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ do
            let ctx = collectionContext colls `mappend` postCtx
            pandocMathCompiler
                >>= loadAndApplyTemplate "templates/post.html"    ctx
                >>= loadAndApplyTemplate "templates/collectionInfo.html" ctx
                >>= loadAndApplyTemplate "templates/draft.html"   ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    --listField "collections" collectionContext (map snd $ collMap colls) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    --constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

----

headContext = mconcat [ parseListMetadata "scripts"
                      , parseListMetadata "styles"
                      , defaultContext 
                      ]

parseListMetadata :: String -> Context a
parseListMetadata s = listField s defaultContext $ do
    identifier <- getUnderlying
    metadata <- getMetadata identifier
    let metas = maybe [] (map trim . splitAll ",") $ M.lookup s metadata
    return $ map (\x -> Item (fromFilePath x) x) metas
