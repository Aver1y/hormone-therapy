{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid (mappend)
import Hakyll
import Hakyll.Core.Compiler.Internal
import Hakyll.Web.Sass (sassCompiler)
import Text.Pandoc (Pandoc (..))
import Text.Pandoc.Legacy.Builder (setMeta)
import Text.Pandoc.Writers.Shared (toTableOfContents)
import Text.CSL.Pandoc (processCites')
import Control.Monad.IO.Class
import Control.Monad
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Filter as P
import Data.Functor.Compose (Compose (..))
import Data.Coerce
import Data.Functor
import qualified Text.Regex.TDFA as X
import System.Process (system)
import System.Exit (ExitCode(..))
import Text.Pandoc.CrossRef (runCrossRef, crossRefBlocks)
import qualified Text.Pandoc.Url2Cite as U2C

readerOptions :: P.ReaderOptions
readerOptions = defaultHakyllReaderOptions
writerOptions :: P.WriterOptions
writerOptions = defaultHakyllWriterOptions

u2cConf :: U2C.Configuration
u2cConf = U2C.def{
  U2C._allowDangling = U2C.AllowDangling,
  U2C._cache = U2C.Cache "citation-cache/cache.json",
  U2C._linkOutput = U2C.Sup
  }

-- | Feeds the pandoc through citeproc and url2cite and adds go-back links to
--   bibliography
pandocFilter :: Pandoc -> IO Pandoc
pandocFilter (Pandoc meta content) =
  fmap addBack . processCites' . crossRef <=< U2C.url2citeWith u2cConf
  $ annotated
  where annotated = Pandoc
          (setMeta "citation-style" ("style.csl" :: String) .
           setMeta "reference-section-title" ("References" :: String) .
           setMeta "link-citations" True .
           setMeta "linkReferences" True .
           setMeta "nameInLink" True .
           setMeta "tableEqns" True $
           meta)
          content
        crossRef :: Pandoc -> Pandoc
        crossRef (Pandoc meta blocks) =
          Pandoc meta $ runCrossRef meta (Just "html") crossRefBlocks blocks
        addBack :: Pandoc -> Pandoc
        addBack (Pandoc meta blocks) = Pandoc meta $ blocks <&>
          \case
            P.Div attrs@("refs", _, _) entries -> P.Div attrs $ entries <&>
              \case
                P.Div attrs [P.Para citation] ->
                  P.Div attrs [P.Para (citation <> goBack)]
                _ -> error "Citation formatting is unexpected"
            r -> r
        goBack = [P.Space, P.Span ("", ["go-back"], [])
          [P.Str "(",
           P.Link ("", [], []) [P.Str "↩"]
                  ("javascript:history.back()", ""),
           P.Str ")"]]

-- | Pandoc inserts a link at the end of each footnote which points back to the
--   referring site, but it is much more intuitive to go back in the browser
--   history, as that takes you back to the exact view you had before. Maybe it
--   would be possible to replace these links inside the blaze html
--   representation, but that doesn't look like it's designed to be inspected
--   and so we'll just use regexes. Ugh
fixFootnoteLinks :: String -> String
fixFootnoteLinks s = go id s
  where
    regex :: String
    regex = "<a [^>]*class=\"footnote-back\"[^>]*>[^<]*</a>"
    goBack :: String
    goBack = " <span class=\"go-back\">(<a href=\"javascript:history.back()\">\
             \↩</a>)</span>"
    go :: (String -> String) -> String -> String
    go done todo = case todo X.=~~ regex of
      Nothing -> done todo
      Just (pre, _ :: String, post) -> go (done . ((pre <> goBack) <>)) post

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match ("css/*.sass" .||. "css/*.scss") $ do
      route $ setExtension "css"
      compile $ fmap compressCss <$> sassCompiler

    match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    match ("posts/**.md" .||. "posts/**.odt" .||. "posts/**.tex" .||. "posts/**.html") $ do
        route $ setExtension "html"
        compile $ do
          pandoc@(Item ident (Pandoc meta content)) <-
             traverse (unsafeCompiler . pandocFilter)
             =<< readPandocWith readerOptions
             =<< getResourceString
          let renderedBody =
                fixFootnoteLinks <$> writePandocWith writerOptions pandoc
              toc = case toTableOfContents writerOptions content of
                      P.BulletList [] -> P.Null
                      P.BulletList [[]] -> P.Null
                      r -> r
              Item _ renderedToc =
                writePandocWith writerOptions
                  (const (Pandoc meta [toc]) <$> pandoc)
              tocCtx = constField "toc" renderedToc
          relativizeUrls
            =<< loadAndApplyTemplate "templates/default.html" postCtx
            =<< loadAndApplyTemplate "templates/post.html" (tocCtx <> postCtx)
                renderedBody

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    titleFieldContext `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    titleFieldContext `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- | This is so that we can get "page name - title" if title is set and
--  "page name" otherwise.
titleFieldContext :: Context a
titleFieldContext = field "titleField" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ case lookupString "title" metadata of
      Just title@(_:_) -> "— " `mappend` title
      _                -> ""

