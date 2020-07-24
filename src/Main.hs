{-# LANGUAGE StandaloneDeriving, DeriveGeneric, OverloadedStrings #-}

module Main where

import Data.BEncode (BEncode(..),bRead,bPack)
import qualified Data.BEncode as BE
import GHC.Generics (Generic(..))
import Data.Aeson (Value(..),ToJSON(..),FromJSON(..),encode,decode,genericToEncoding,defaultOptions,sumEncoding,SumEncoding(..))
import Data.ByteString.Lazy.Char8 (ByteString(..))
import qualified Data.ByteString.Lazy.UTF8 as U8 (fromString)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy as B8
import qualified Data.ByteString as SB8 (hGetContents)
import Data.Aeson.Encoding (lazyText,null_)
import Data.Text.Lazy.Encoding (decodeUtf8',decodeUtf8)
import Data.Either (fromRight)
import System.Environment (getArgs)
import System.IO (withBinaryFile,IOMode(..))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Digest.Pure.SHA (sha1)
import Data.ByteArray.Encoding (convertToBase,Base(..))
import Control.Applicative ((<|>))

deriving instance Generic BEncode

instance ToJSON ByteString where
  toJSON bs = fromRight "<data>" (toJSON <$> decodeUtf8' bs)
  toEncoding bs = fromRight null_ (lazyText <$> decodeUtf8' bs)

instance ToJSON BEncode where
  toEncoding = genericToEncoding defaultOptions { sumEncoding = UntaggedValue }

base64Encode bs = B8.fromStrict $ convertToBase Base64 bs'
  where bs' = B8.toStrict bs

dataURI mime charset bs = B8.concat ["data:",mime',";charset=",charset',";base64,",b64data]
  where mime'    = fromMaybe "application/octet-stream" mime
        charset' = fromMaybe "binary" charset
        b64data  = base64Encode bs

bencodeLookup k (BDict m) = M.lookup k m
bencodeLookup k _ = Nothing

infoHash :: BEncode -> Maybe BEncode
infoHash be = torrent_hash <|> magnet_hash
  where torrent_hash = BString . BL.pack . show . sha1 . bPack <$> bencodeLookup "info" be
        magnet_hash = do mi <- bencodeLookup "magnet-info" be
                         ih <- bencodeLookup "info_hash" mi
                         case ih of
                              BString bs -> return (bsToHex bs)
                              _          -> Nothing
        bsToHex bs = BString $ B8.fromStrict ( convertToBase Base16 (B8.toStrict bs ) ) 

bdictInsert :: String -> BEncode -> BEncode -> BEncode
bdictInsert k v (BDict m)  = BDict $ M.insert k v m
bdictInsert _ _ bd         = bd

addInfoHash be = maybe be (\bs -> bdictInsert "hash" bs be) (infoHash be )

bytesToTorrent file bs = bdictInsert "filename" (bString file) . addInfoHash <$> bRead bs

bString str = BString ( U8.fromString str )

main = do files <- getArgs
          mapM_ jsonify files
  where jsonify file = withBinaryFile file ReadMode 
                   (\h -> do bs   <- B8.fromStrict <$> SB8.hGetContents h 
                             let json = encode <$> bytesToTorrent file bs
                             maybe mempty BL.putStrLn json )
