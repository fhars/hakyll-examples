{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Main where

import Prelude hiding (id,lookup)
import Control.Category (id)
import Control.Applicative ((<|>))
import Control.Arrow ((>>>), (***), (>>^), arr)
import Control.Exception

import Data.Map (empty, fromList, union, lookup, findWithDefault)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, mconcat)
import Data.Typeable (Typeable)
import Data.Binary (Binary)

import System.Directory (copyFile)
import System.FilePath.Posix ((<.>), splitExtension)
import Graphics.Transform.Magick.Images
import Graphics.Transform.Magick.Types (getImage, FilterTypes (..), HImage (..), HImage_ (..), PixelPacket (..))

import Foreign.ForeignPtr
import Foreign.Storable (peek)

import Graphics.Exif

import Hakyll

main :: IO ()
main = do
  initializeMagick
  hakyll $ do
    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Compress CSS
    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    -- Scale images
    match "images/**.jpg" $ do
      group "imgs" $ do
        route   idRoute
        compile $ maxImageDim 320
      group "origs" $ do
        route $ gsubRoute ".jpg$" (const ".orig.jpg")
        compile copyFileCompiler
      group "thumbs" $ do
        route $ gsubRoute ".jpg" (const ".thumb.jpg")
        compile $ maxImageDim 80
      route   $ setExtension ".html"
      compile $ exifData
        >>> imagePage
        >>> applyTemplateCompiler "templates/image_page.html"

newtype MaxImageDim = MaxImageDim {unMaxImageDim :: (Int, FilePath)}
                 deriving (Show, Eq, Ord, Binary, Typeable)

instance Writable MaxImageDim where
  write dst (MaxImageDim (dim, src)) =  do
    img <- readImage src
    (wd, ht) <- getDims img
    if wd <= dim && ht <= dim then
      copyFile src dst
    else  
      do
        let wdScale = toRational wd / toRational dim
        let htScale = toRational ht / toRational dim
        let (w,h) = 
             if htScale > wdScale then
               (floor $ fromIntegral wd / htScale, dim)
             else
               (dim, floor $ fromIntegral ht / wdScale)
        writeImage dst $ 
          gammaImage (PixelPacket 0.454545 0.454545 0.454545 0.454545) $
          resizeImage w h SincFilter 1.1 $
          gammaImage (PixelPacket 2.2 2.2 2.2 2.2) img
          
getDims :: HImage -> IO (Int, Int)
getDims himg =
  withForeignPtr (getImage himg) $ \p -> do
    img <- peek p
    return $ (fromIntegral $ columns img, fromIntegral $ rows img)

maxImageDim :: Int -> Compiler Resource MaxImageDim
maxImageDim dim = getIdentifier >>^ maxDim dim
  where
    maxDim dim resource = MaxImageDim (dim, toFilePath resource)

exifData :: Compiler Resource (Page FilePath)
exifData = getIdentifier >>> unsafeCompiler addExifAsAttributes
  where
   addExifAsAttributes resource = 
     do 
       exif <- try(fromFile $ fp) :: IO (Either SomeException Exif)
       case exif of
          Right e ->
            do
              tags <-  allTags e
              return $ Page (fromList tags) fp
          Left _ ->
            return $ Page empty fp
     where fp = toFilePath resource
     
imagePage :: Compiler (Page FilePath) (Page String)
imagePage = arr (\p -> fromMap $ newMetadata $ toMap p)  >>> addDefaultFields
  where
    newMetadata m = 
      let url = findWithDefault "#" "body" m
          (base, ext) = splitExtension url
          fullurl = base <.> "orig" <.> ext
          date =  getDate m
          new = fromList [("alt", ""),
                          ("date", date),
                          ("fullurl", fullurl),
                          ("imgurl", url)]
      in
      union new m
    getDate m = 
      fromMaybe "UnknownDate" $
        lookup "DateTimeOriginal" m
        <|> lookup "DateTimeDigitized" m
        <|> lookup "DateTime" m
