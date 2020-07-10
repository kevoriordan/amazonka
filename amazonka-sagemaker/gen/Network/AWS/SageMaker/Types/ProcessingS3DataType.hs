{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3DataType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingS3DataType (
  ProcessingS3DataType (
    ..
    , PSDTManifestFile
    , PSDTS3Prefix
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProcessingS3DataType = ProcessingS3DataType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern PSDTManifestFile :: ProcessingS3DataType
pattern PSDTManifestFile = ProcessingS3DataType' "ManifestFile"

pattern PSDTS3Prefix :: ProcessingS3DataType
pattern PSDTS3Prefix = ProcessingS3DataType' "S3Prefix"

{-# COMPLETE
  PSDTManifestFile,
  PSDTS3Prefix,
  ProcessingS3DataType' #-}

instance FromText ProcessingS3DataType where
    parser = (ProcessingS3DataType' . mk) <$> takeText

instance ToText ProcessingS3DataType where
    toText (ProcessingS3DataType' ci) = original ci

-- | Represents an enum of /known/ $ProcessingS3DataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProcessingS3DataType where
    toEnum i = case i of
        0 -> PSDTManifestFile
        1 -> PSDTS3Prefix
        _ -> (error . showText) $ "Unknown index for ProcessingS3DataType: " <> toText i
    fromEnum x = case x of
        PSDTManifestFile -> 0
        PSDTS3Prefix -> 1
        ProcessingS3DataType' name -> (error . showText) $ "Unknown ProcessingS3DataType: " <> original name

-- | Represents the bounds of /known/ $ProcessingS3DataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProcessingS3DataType where
    minBound = PSDTManifestFile
    maxBound = PSDTS3Prefix

instance Hashable     ProcessingS3DataType
instance NFData       ProcessingS3DataType
instance ToByteString ProcessingS3DataType
instance ToQuery      ProcessingS3DataType
instance ToHeader     ProcessingS3DataType

instance ToJSON ProcessingS3DataType where
    toJSON = toJSONText

instance FromJSON ProcessingS3DataType where
    parseJSON = parseJSONText "ProcessingS3DataType"
