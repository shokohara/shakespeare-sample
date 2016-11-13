{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib where

import qualified Data.ByteString as B
import qualified Data.Text as T
--import Data.Text (Text, pack, unpack)
import Data.Text.Lazy.Builder
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import GHC.Generics (Generic)
import Text.Shakespeare.Text
import qualified Data.Text.Lazy as TL
--import Language.Haskell.TH

data Element = Element { version, url, rspecVersion :: String } deriving (Show, Eq, Generic)

instance FromJSON Element where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }

parseYaml :: B.ByteString -> Maybe [Element]
parseYaml x = Y.decode x :: Maybe [Element]

--loadjs :: String -> IO B.ByteString
--loadjs x = B.readFile x

toElement :: String -> IO (Maybe [Element])
toElement x = B.readFile x >>= return . parseYaml

-- hoge :: QuasiQuoter
-- hoge = QuasiQuoter {
--   quoteExp = liftE . stringL
-- }

f :: Builder -> T.Text
f x = TL.toStrict . Data.Text.Lazy.Builder.toLazyText . toText $ x

dockerfile :: Element -> T.Text
dockerfile e = do
--  let x = version e
--  let rubyVersion2 = "2.3" :: String
--  let rspecVersion = "3.4.0" :: String
  let x = $(textFile "Dockerfile.template") e :: Builder
  f x
--  T.pack ""
--  let a = textFile "Dockerfile.template"
--  T.pack . show $ [hoge|a|]
