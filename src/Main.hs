{-# LANGUAGE StandaloneDeriving, DeriveGeneric, OverloadedStrings #-}

module Main where

import Data.BEncode (BEncode(..),bRead,bPack)
import qualified Data.BEncode as BE
import GHC.Generics (Generic(..))
import Data.Aeson (Value(..),ToJSON(..),FromJSON(..),encode,decode,genericToEncoding,defaultOptions,sumEncoding,SumEncoding(..))
import Data.ByteString.Lazy.Char8 (ByteString(..))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson.Encoding (lazyText,null_)
import Data.Text.Lazy.Encoding (decodeUtf8',decodeUtf8)
import Data.Either (fromRight)
import System.Environment (getArgs)
import System.Exit (die)
import Data.Maybe (catMaybes,fromMaybe)
import qualified Data.Map.Strict as M
import Data.Digest.Pure.SHA (sha1)
import Data.ByteArray.Encoding (convertToBase,Base(..))

deriving instance Generic BEncode

instance ToJSON ByteString where
  toJSON bs = fromRight "<data>" (toJSON <$> decodeUtf8' bs)
--toEncoding bs = lazyText $ fromRight (decodeUtf8 $ dataURI Nothing Nothing bs) (decodeUtf8' bs)
  toEncoding bs = fromRight null_ (lazyText <$> decodeUtf8' bs)

instance ToJSON BEncode where
  toEncoding = genericToEncoding defaultOptions { sumEncoding = UntaggedValue }

base64Encode bs = BL.fromStrict $ convertToBase Base64 bs'
  where bs' = BL.toStrict bs

dataURI mime charset bs = BL.concat ["data:",mime',";charset=",charset',";base64,",b64data]
  where mime'    = fromMaybe "application/octet-stream" mime
        charset' = fromMaybe "binary" charset
        b64data  = base64Encode bs

readTorrentFile path = bRead <$> BL.readFile path 

bencodeLookup k (BDict m) = M.lookup k m
bencodeLookup k _ = Nothing

infoHash :: BEncode -> Maybe String
infoHash be = show . sha1 . bPack <$> bencodeLookup "info" be

addInfoHash be@(BDict m) = do hash <- BString . BL.pack <$> infoHash be
                              let new = BDict (M.insert "hash" hash m) 
                              return new
addInfoHash _ = Nothing

bytesToInfo bs = bRead bs >>= addInfoHash

main = do args <- getArgs
          fs   <- mapM BL.readFile args
          let ts = map bytesToInfo fs
          mapM_ ( BL.putStrLn . encode ) (catMaybes ts)
