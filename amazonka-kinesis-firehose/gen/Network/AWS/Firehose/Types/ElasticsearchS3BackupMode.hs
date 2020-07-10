{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ElasticsearchS3BackupMode (
  ElasticsearchS3BackupMode (
    ..
    , AllDocuments
    , FailedDocumentsOnly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ElasticsearchS3BackupMode = ElasticsearchS3BackupMode' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern AllDocuments :: ElasticsearchS3BackupMode
pattern AllDocuments = ElasticsearchS3BackupMode' "AllDocuments"

pattern FailedDocumentsOnly :: ElasticsearchS3BackupMode
pattern FailedDocumentsOnly = ElasticsearchS3BackupMode' "FailedDocumentsOnly"

{-# COMPLETE
  AllDocuments,
  FailedDocumentsOnly,
  ElasticsearchS3BackupMode' #-}

instance FromText ElasticsearchS3BackupMode where
    parser = (ElasticsearchS3BackupMode' . mk) <$> takeText

instance ToText ElasticsearchS3BackupMode where
    toText (ElasticsearchS3BackupMode' ci) = original ci

-- | Represents an enum of /known/ $ElasticsearchS3BackupMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ElasticsearchS3BackupMode where
    toEnum i = case i of
        0 -> AllDocuments
        1 -> FailedDocumentsOnly
        _ -> (error . showText) $ "Unknown index for ElasticsearchS3BackupMode: " <> toText i
    fromEnum x = case x of
        AllDocuments -> 0
        FailedDocumentsOnly -> 1
        ElasticsearchS3BackupMode' name -> (error . showText) $ "Unknown ElasticsearchS3BackupMode: " <> original name

-- | Represents the bounds of /known/ $ElasticsearchS3BackupMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ElasticsearchS3BackupMode where
    minBound = AllDocuments
    maxBound = FailedDocumentsOnly

instance Hashable     ElasticsearchS3BackupMode
instance NFData       ElasticsearchS3BackupMode
instance ToByteString ElasticsearchS3BackupMode
instance ToQuery      ElasticsearchS3BackupMode
instance ToHeader     ElasticsearchS3BackupMode

instance ToJSON ElasticsearchS3BackupMode where
    toJSON = toJSONText

instance FromJSON ElasticsearchS3BackupMode where
    parseJSON = parseJSONText "ElasticsearchS3BackupMode"
