{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StorageClassAnalysisSchemaVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.StorageClassAnalysisSchemaVersion (
  StorageClassAnalysisSchemaVersion (
    ..
    , V1
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data StorageClassAnalysisSchemaVersion = StorageClassAnalysisSchemaVersion' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern V1 :: StorageClassAnalysisSchemaVersion
pattern V1 = StorageClassAnalysisSchemaVersion' "V_1"

{-# COMPLETE
  V1,
  StorageClassAnalysisSchemaVersion' #-}

instance FromText StorageClassAnalysisSchemaVersion where
    parser = (StorageClassAnalysisSchemaVersion' . mk) <$> takeText

instance ToText StorageClassAnalysisSchemaVersion where
    toText (StorageClassAnalysisSchemaVersion' ci) = original ci

-- | Represents an enum of /known/ $StorageClassAnalysisSchemaVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StorageClassAnalysisSchemaVersion where
    toEnum i = case i of
        0 -> V1
        _ -> (error . showText) $ "Unknown index for StorageClassAnalysisSchemaVersion: " <> toText i
    fromEnum x = case x of
        V1 -> 0
        StorageClassAnalysisSchemaVersion' name -> (error . showText) $ "Unknown StorageClassAnalysisSchemaVersion: " <> original name

-- | Represents the bounds of /known/ $StorageClassAnalysisSchemaVersion.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StorageClassAnalysisSchemaVersion where
    minBound = V1
    maxBound = V1

instance Hashable     StorageClassAnalysisSchemaVersion
instance NFData       StorageClassAnalysisSchemaVersion
instance ToByteString StorageClassAnalysisSchemaVersion
instance ToQuery      StorageClassAnalysisSchemaVersion
instance ToHeader     StorageClassAnalysisSchemaVersion

instance FromXML StorageClassAnalysisSchemaVersion where
    parseXML = parseXMLText "StorageClassAnalysisSchemaVersion"

instance ToXML StorageClassAnalysisSchemaVersion where
    toXML = toXMLText
