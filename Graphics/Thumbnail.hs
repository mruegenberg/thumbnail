-- Based on http://hackage.haskell.org/package/thumbnail

module Graphics.Thumbnail
       ( ImageFormat(..)
       , Thumbnail(..)
       , mkThumbnail
       , mkThumbnail'
       , defaultBounds) where

import Prelude
import Graphics.GD
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L

data ImageFormat = Gif | Jpeg | Png

-- | Convert `ImageFormat` to a mime type
formatToType :: ImageFormat -> String
formatToType Gif  = "image/gif"
formatToType Jpeg = "image/jpeg"
formatToType Png  = "image/png"

data Thumbnail = Thumbnail { fmt :: ImageFormat     -- ^ Image Format Type
                           , img :: Image           -- ^ Thumbnail Image
                           , sz  :: Size            -- ^ Thumbnail Size
                           , lbs :: L.ByteString    -- ^ Thumbnail Data
                           , orgImg :: Image        -- ^ Original Image
                           , orgSZ :: Size          -- ^ Original Size
                           , saveFile :: FilePath -> IO ()
                           }

-- | Create a thumbnails with the default size
mkThumbnail :: L.ByteString -> IO (Either String Thumbnail)
mkThumbnail = mkThumbnail' defaultBounds

-- | Create a thumbnail from a specific subregion of the image
mkThumbnail' :: ((Int,Int),(Int,Int)) -> L.ByteString -> IO (Either String Thumbnail)
mkThumbnail' sizeBounds = thumbnail . L.unpack
  where
    thumbnail ws | length ws >= 3 = thumbnail' ws -- FIXME!
                 | otherwise = return $ Left "unsupported image format"
    
    thumbnail' ws@(0xff:0xd8:_) = thumbnailJpeg ws
    thumbnail' ws@(0x89:0x50:_) = thumbnailPng ws
    thumbnail' ws@(0x47:0x49:0x46:_) = thumbnailGif ws
    thumbnail' _ = return $ Left "unsupported image format"
    
    thumbnailJpeg ws = do
      src <- loadJpegByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize sizeBounds size
      thm <- uncurry resizeImage size' dest
      bs <- saveJpegByteString (-1) thm
      let save fp = saveJpegFile (-1) fp thm
      return $ Right Thumbnail { fmt=Jpeg
                               , img=thm
                               , sz=size'
                               , lbs=strictToLazy bs
                               , orgImg=src
                               , orgSZ=size
                               , saveFile=save
                               }
    
    thumbnailPng ws = do
      src <- loadPngByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize sizeBounds size
      thm <- uncurry resizeImage size' dest
      bs <- savePngByteString thm
      let save fp = savePngFile fp thm
      return $ Right Thumbnail { fmt=Png
                               , img=thm
                               , sz=size'
                               , lbs=strictToLazy bs
                               , orgImg=src
                               , orgSZ=size
                               , saveFile=save
                               }
      
    thumbnailGif ws = do
      src <- loadGifByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize sizeBounds size
      thm <- uncurry resizeImage size' dest
      bs <- saveGifByteString thm
      let save fp = saveGifFile fp thm
      return $ Right Thumbnail { fmt=Gif
                               , img=thm
                               , sz=size'
                               , lbs=strictToLazy bs
                               , orgImg=src
                               , orgSZ=size
                               , saveFile=save
                               }
        
    strictToLazy = L.pack . BS.unpack
    
newSize :: ((Int,Int),(Int,Int)) -> Size -> Size
newSize ((wMin,hMin),(wMax,hMax)) (w, h) =
  let wForMaxH = hMax*w`div`h
      hForMaxW = wMax*h`div`w
      maximizedSize | wForMaxH <= wMax = (wForMaxH, hMax)
                    | otherwise = (wMax, hForMaxW)
      wForMinH = hMin*w`div`h
      hForMinW = wMin*h`div`w
      minimizedSize | wForMinH >= wMin = (wForMinH, wMin)
                    | hForMinW >= hMin = (wMin, hForMinW)
                    -- in this case, the aspect ration can't be maintained, 
                    -- but we assume that wMin/hMin should be respected in any case. 
                    -- Ideally, we want cropping instead, so that the image is not distorted
                    | otherwise = (wMin,hMin) 
  in  if fst maximizedSize > wMin && snd maximizedSize > hMin 
      then maximizedSize 
      else minimizedSize

defaultBounds :: ((Int,Int),(Int,Int))
defaultBounds = ((20,20),(60,60))
