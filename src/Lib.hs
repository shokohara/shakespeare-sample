{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import GHC.Generics (Generic)
import Text.Shakespeare.Text
import qualified Data.Text.Lazy as TL

data Element = Element { version, url, rspecVersion :: String } deriving (Show, Eq, Generic)

instance FromJSON Element where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }

parseYaml :: B.ByteString -> Maybe [Element]
parseYaml x = Y.decode x :: Maybe [Element]

toElement :: String -> IO (Maybe [Element])
toElement x = fmap parseYaml (B.readFile x)

renderDockerfile :: Element -> T.Text
renderDockerfile e = f . $(textFile "Dockerfile.template") $ e
  where
    f :: Builder -> T.Text
    f = TL.toStrict . Data.Text.Lazy.Builder.toLazyText . toText
--renderDockerfile e = do
--  let x = $(textFile "Dockerfile.template") e :: Builder
--  f x
--  where
--    f :: Builder -> T.Text
--    f = TL.toStrict . Data.Text.Lazy.Builder.toLazyText . toText

