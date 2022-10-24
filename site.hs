--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Codec.Picture (decodeImage)
import Codec.Picture.Saving (imageToJpg)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import qualified Data.Set as S
import Data.String (fromString)
import Data.Tree (flatten)
import Hakyll
import Hakyll.Core.Compiler
import System.FilePath
import Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match (fromGlob "fonts/*" .||. otherImagePattern) $ do
    route idRoute
    compile copyFileCompiler

  match jpgOrPngPattern $ do
    route idRoute
    compile $
      loadImage
        >>= compressImageCompiler 60

  match "css/**" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/**" $ compile templateBodyCompiler

  match (fromList ["about.org"]) $ do
    route $ setExtension "html"
    compile $
      pandocMathCompiler
        >>= loadAndApplyTemplate "templates/article-card.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- build up tags
  allTags <- buildTags postPattern (fromCapture "tags/*.html")

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

  -- paginate
  pag <- buildPaginateWith grouper postPattern makeId

  paginateRules pag $ \pageNum pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let paginateCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` paginateContext pag pageNum
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" paginateCtx
        >>= loadAndApplyTemplate "templates/default.html" paginateCtx
        >>= relativizeUrls

  match postPattern $ do
    route $ setExtension "html"
    compile $
      pandocMathCompiler
        >>= loadAndApplyTemplate "templates/article-content.html" postCtx
        >>= loadAndApplyTemplate "templates/article-card.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postPattern
      let indexCtx =
            listField "posts" postCtx (return $ take 7 posts)
              `mappend` defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

--------------------------------------------------------------------------------
postPattern :: Pattern
postPattern = do
  let extensions = [".md", ".markdown", ".org", ".rst"]
      patterns = map (fromGlob . ("posts/**" ++)) extensions
  foldl1 (.||.) patterns

jpgOrPngPattern :: Pattern
jpgOrPngPattern = do
  let extensions = [".jpg", ".jpeg", ".png"]
      patterns =
        map (fromGlob . ("posts/**" ++)) extensions
          <> map (fromGlob . ("images/**" ++)) extensions
  foldl1 (.||.) patterns

otherImagePattern :: Pattern
otherImagePattern = do
  let extensions = [".ico", ".bmp", ".gif", ".tif", ".tiff"]
      patterns =
        map (fromGlob . ("posts/**" ++)) extensions
          <> map (fromGlob . ("images/**" ++)) extensions
  foldl1 (.||.) patterns

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  tagsCtx
    `mappend` coverCtx
    `mappend` dateField "date" "%B %e, %Y"
    `mappend` defaultContext

coverCtx :: Context String
coverCtx = field "cover" $ \itemStr -> do
  let item = itemIdentifier itemStr
      path = toFilePath item
  coverRelativeUrl <- fromMaybe "" <$> getMetadataField item "cover"
  return $ "/" ++ takeDirectory path ++ "/" ++ coverRelativeUrl

tagsCtx :: Context String
tagsCtx =
  listFieldWith
    "tags"
    ctx
    $ \itemStr -> do
      tags <- getMetadataField (itemIdentifier itemStr) "tags"
      return $ case tags of
        Just lst -> map (mkTagItem . trim) $ splitAll "," lst
        Nothing -> []
  where
    ctx = field "tag" (return . itemBody)
    mkTagItem tag =
      Item
        { itemIdentifier = fromString ("tag/" ++ tag),
          itemBody = tag
        }

--------------------------------------------------------------------------------
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  let mathExtensions =
        [ Ext_tex_math_dollars,
          Ext_tex_math_double_backslash,
          Ext_latex_macros
        ]
      defaultExtensions =
        writerExtensions defaultHakyllWriterOptions
      newExtensions =
        defaultExtensions
          `mappend` pandocExtensions
          `mappend` extensionsFromList mathExtensions
      writerOptions =
        defaultHakyllWriterOptions
          { writerExtensions = newExtensions,
            writerHTMLMathMethod = MathJax ""
          }
   in pandocCompilerWith defaultHakyllReaderOptions writerOptions

--------------------------------------------------------------------------------
grouper :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [[Identifier]]
grouper = fmap (paginateEvery 30) . sortRecentFirst

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "posts/page/" ++ show pageNum ++ "/index.html"

--------------------------------------------------------------------------------
loadImage :: Compiler (Item ByteString)
loadImage = do
  fmap toStrict <$> getResourceLBS

compressImageCompiler :: Int -> Item ByteString -> Compiler (Item ByteString)
compressImageCompiler quality = return . fmap (compressImage quality)

-- compress and save image as jpg
compressImage :: Int -> ByteString -> ByteString
compressImage quality src =
  case decodeImage src of
    Left s -> error s
    Right dynImage -> toStrict (imageToJpg quality dynImage)