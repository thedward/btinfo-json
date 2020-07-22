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
import Data.Aeson.Encoding (lazyText,null_)
import Data.Text.Lazy.Encoding (decodeUtf8',decodeUtf8)
import Data.Either (fromRight)
import System.Environment (getArgs)
import System.Exit (die)
import Data.Maybe (catMaybes,fromMaybe)
import qualified Data.Map.Strict as M
import Data.Digest.Pure.SHA (sha1)
import Data.ByteArray.Encoding (convertToBase,Base(..))
import Control.Applicative ((<|>))

deriving instance Generic BEncode

instance ToJSON ByteString where
  toJSON bs = fromRight "<data>" (toJSON <$> decodeUtf8' bs)
--toEncoding bs = lazyText $ fromRight (decodeUtf8 $ dataURI Nothing Nothing bs) (decodeUtf8' bs)
  toEncoding bs = fromRight null_ (lazyText <$> decodeUtf8' bs)

instance ToJSON BEncode where
  toEncoding = genericToEncoding defaultOptions { sumEncoding = UntaggedValue }

base64Encode bs = B8.fromStrict $ convertToBase Base64 bs'
  where bs' = B8.toStrict bs

dataURI mime charset bs = B8.concat ["data:",mime',";charset=",charset',";base64,",b64data]
  where mime'    = fromMaybe "application/octet-stream" mime
        charset' = fromMaybe "binary" charset
        b64data  = base64Encode bs

readTorrentFile path = bRead <$> B8.readFile path 

bencodeLookup k (BDict m) = M.lookup k m
bencodeLookup k _ = Nothing

infoHash :: BEncode -> Maybe String
infoHash be = torrent_hash <|> magnet_hash
  where torrent_hash = show . sha1 . bPack <$> bencodeLookup "info" be
        magnet_hash = do mi <- bencodeLookup "magnet-info" be
                         ih <- bencodeLookup "info_hash" mi
                         case ih of
                              BString bs -> return (bsToHex bs)
                              _          -> Nothing
        bsToHex bs = BL.unpack $ B8.fromStrict ( convertToBase Base16 (B8.toStrict bs ) ) 


addInfoHash be@(BDict m) = case BString . BL.pack <$> infoHash be of
                                Nothing   -> return be
                                Just hash -> return ( BDict (M.insert "hash" hash m) )

addInfoHash _ = Nothing

addFileName f (BDict m) = BDict ( M.insert "filename" (BString f) m )
addFileName _ bd = bd

bytesToInfo bs = bRead bs >>= addInfoHash

main = do args <- getArgs
          fs   <- mapM BL.readFile args
          let ts  = map bytesToInfo fs
          let ts' = zipWith ( \f bd ->  addFileName f <$> bd ) (map U8.fromString args) ts
          mapM_ ( BL.putStrLn . encode ) (catMaybes ts')
