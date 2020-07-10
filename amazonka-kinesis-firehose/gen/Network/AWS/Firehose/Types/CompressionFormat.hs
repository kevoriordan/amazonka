{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.CompressionFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.CompressionFormat (
  CompressionFormat (
    ..
    , Gzip
    , HadoopSnappy
    , Snappy
    , Uncompressed
    , Zip
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CompressionFormat = CompressionFormat' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Gzip :: CompressionFormat
pattern Gzip = CompressionFormat' "GZIP"

pattern HadoopSnappy :: CompressionFormat
pattern HadoopSnappy = CompressionFormat' "HADOOP_SNAPPY"

pattern Snappy :: CompressionFormat
pattern Snappy = CompressionFormat' "Snappy"

pattern Uncompressed :: CompressionFormat
pattern Uncompressed = CompressionFormat' "UNCOMPRESSED"

pattern Zip :: CompressionFormat
pattern Zip = CompressionFormat' "ZIP"

{-# COMPLETE
  Gzip,
  HadoopSnappy,
  Snappy,
  Uncompressed,
  Zip,
  CompressionFormat' #-}

instance FromText CompressionFormat where
    parser = (CompressionFormat' . mk) <$> takeText

instance ToText CompressionFormat where
    toText (CompressionFormat' ci) = original ci

-- | Represents an enum of /known/ $CompressionFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CompressionFormat where
    toEnum i = case i of
        0 -> Gzip
        1 -> HadoopSnappy
        2 -> Snappy
        3 -> Uncompressed
        4 -> Zip
        _ -> (error . showText) $ "Unknown index for CompressionFormat: " <> toText i
    fromEnum x = case x of
        Gzip -> 0
        HadoopSnappy -> 1
        Snappy -> 2
        Uncompressed -> 3
        Zip -> 4
        CompressionFormat' name -> (error . showText) $ "Unknown CompressionFormat: " <> original name

-- | Represents the bounds of /known/ $CompressionFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CompressionFormat where
    minBound = Gzip
    maxBound = Zip

instance Hashable     CompressionFormat
instance NFData       CompressionFormat
instance ToByteString CompressionFormat
instance ToQuery      CompressionFormat
instance ToHeader     CompressionFormat

instance ToJSON CompressionFormat where
    toJSON = toJSONText

instance FromJSON CompressionFormat where
    parseJSON = parseJSONText "CompressionFormat"
