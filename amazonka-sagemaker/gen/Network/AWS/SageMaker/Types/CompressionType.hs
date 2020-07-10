{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CompressionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CompressionType (
  CompressionType (
    ..
    , CTGzip
    , CTNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CompressionType = CompressionType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CTGzip :: CompressionType
pattern CTGzip = CompressionType' "Gzip"

pattern CTNone :: CompressionType
pattern CTNone = CompressionType' "None"

{-# COMPLETE
  CTGzip,
  CTNone,
  CompressionType' #-}

instance FromText CompressionType where
    parser = (CompressionType' . mk) <$> takeText

instance ToText CompressionType where
    toText (CompressionType' ci) = original ci

-- | Represents an enum of /known/ $CompressionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CompressionType where
    toEnum i = case i of
        0 -> CTGzip
        1 -> CTNone
        _ -> (error . showText) $ "Unknown index for CompressionType: " <> toText i
    fromEnum x = case x of
        CTGzip -> 0
        CTNone -> 1
        CompressionType' name -> (error . showText) $ "Unknown CompressionType: " <> original name

-- | Represents the bounds of /known/ $CompressionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CompressionType where
    minBound = CTGzip
    maxBound = CTNone

instance Hashable     CompressionType
instance NFData       CompressionType
instance ToByteString CompressionType
instance ToQuery      CompressionType
instance ToHeader     CompressionType

instance ToJSON CompressionType where
    toJSON = toJSONText

instance FromJSON CompressionType where
    parseJSON = parseJSONText "CompressionType"
