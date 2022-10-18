--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import qualified Data.Set as S
import Data.String (fromString)
import Hakyll
import Text.Pandoc.Options
import Data.Tree (flatten)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match (fromGlob "images/*" .||. fromGlob "fonts/*") $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateBodyCompiler

  match (fromList ["about.org"]) $ do
    route $ setExtension "html"
    compile $
      pandocMathCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- build up tags
  allTags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules allTags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx =
            constField "title" title
              `mappend` listField "posts" postCtx (return posts)
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocMathCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return $ take 7 posts)
              `mappend` defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  -- paginate
  pag <- buildPaginateWith grouper "posts/*" makeId

  paginateRules pag $ \pageNum pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let paginateCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" ("Page " ++ show pageNum)
              `mappend` paginateContext pag pageNum
              `mappend` defaultContext 
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" paginateCtx
        >>= loadAndApplyTemplate "templates/default.html" paginateCtx
        >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  tagsCtx
    `mappend` dateField "date" "%B %e, %Y"
    `mappend` defaultContext

tagsCtx :: Context String
tagsCtx =
  listFieldWith
    "tags"
    ctx
    ( \item -> do
        tags <- getMetadataField (itemIdentifier item) "tags"

        return $ case tags of
          Just lst -> map (mkTagItem . trim) $ splitAll "," lst
          Nothing -> []
    )
  where
    ctx = field "tag" (return . itemBody)
    mkTagItem tag =
      Item
        { itemIdentifier = fromString ("tag/" ++ tag),
          itemBody = tag
        }

--------------------------------------------------------------------------------
pandocMathCompiler =
  let mathExtensions =
        [ Ext_tex_math_dollars,
          Ext_tex_math_double_backslash,
          Ext_latex_macros
        ]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = defaultExtensions `mappend` pandocExtensions `mappend` extensionsFromList mathExtensions
      writerOptions =
        defaultHakyllWriterOptions
          { writerExtensions = newExtensions,
            writerHTMLMathMethod = MathJax ""
          }
   in pandocCompilerWith defaultHakyllReaderOptions writerOptions

--------------------------------------------------------------------------------
grouper :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [[Identifier]]
grouper = fmap (paginateEvery 10) . sortRecentFirst

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "posts/" ++ show pageNum ++ "/index.html"