{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryOptionalField
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.InventoryOptionalField (
  InventoryOptionalField (
    ..
    , FieldETag
    , FieldEncryptionStatus
    , FieldIntelligentTieringAccessTier
    , FieldIsMultipartUploaded
    , FieldLastModifiedDate
    , FieldObjectLockLegalHoldStatus
    , FieldObjectLockMode
    , FieldObjectLockRetainUntilDate
    , FieldReplicationStatus
    , FieldSize
    , FieldStorageClass
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data InventoryOptionalField = InventoryOptionalField' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern FieldETag :: InventoryOptionalField
pattern FieldETag = InventoryOptionalField' "ETag"

pattern FieldEncryptionStatus :: InventoryOptionalField
pattern FieldEncryptionStatus = InventoryOptionalField' "EncryptionStatus"

pattern FieldIntelligentTieringAccessTier :: InventoryOptionalField
pattern FieldIntelligentTieringAccessTier = InventoryOptionalField' "IntelligentTieringAccessTier"

pattern FieldIsMultipartUploaded :: InventoryOptionalField
pattern FieldIsMultipartUploaded = InventoryOptionalField' "IsMultipartUploaded"

pattern FieldLastModifiedDate :: InventoryOptionalField
pattern FieldLastModifiedDate = InventoryOptionalField' "LastModifiedDate"

pattern FieldObjectLockLegalHoldStatus :: InventoryOptionalField
pattern FieldObjectLockLegalHoldStatus = InventoryOptionalField' "ObjectLockLegalHoldStatus"

pattern FieldObjectLockMode :: InventoryOptionalField
pattern FieldObjectLockMode = InventoryOptionalField' "ObjectLockMode"

pattern FieldObjectLockRetainUntilDate :: InventoryOptionalField
pattern FieldObjectLockRetainUntilDate = InventoryOptionalField' "ObjectLockRetainUntilDate"

pattern FieldReplicationStatus :: InventoryOptionalField
pattern FieldReplicationStatus = InventoryOptionalField' "ReplicationStatus"

pattern FieldSize :: InventoryOptionalField
pattern FieldSize = InventoryOptionalField' "Size"

pattern FieldStorageClass :: InventoryOptionalField
pattern FieldStorageClass = InventoryOptionalField' "StorageClass"

{-# COMPLETE
  FieldETag,
  FieldEncryptionStatus,
  FieldIntelligentTieringAccessTier,
  FieldIsMultipartUploaded,
  FieldLastModifiedDate,
  FieldObjectLockLegalHoldStatus,
  FieldObjectLockMode,
  FieldObjectLockRetainUntilDate,
  FieldReplicationStatus,
  FieldSize,
  FieldStorageClass,
  InventoryOptionalField' #-}

instance FromText InventoryOptionalField where
    parser = (InventoryOptionalField' . mk) <$> takeText

instance ToText InventoryOptionalField where
    toText (InventoryOptionalField' ci) = original ci

-- | Represents an enum of /known/ $InventoryOptionalField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InventoryOptionalField where
    toEnum i = case i of
        0 -> FieldETag
        1 -> FieldEncryptionStatus
        2 -> FieldIntelligentTieringAccessTier
        3 -> FieldIsMultipartUploaded
        4 -> FieldLastModifiedDate
        5 -> FieldObjectLockLegalHoldStatus
        6 -> FieldObjectLockMode
        7 -> FieldObjectLockRetainUntilDate
        8 -> FieldReplicationStatus
        9 -> FieldSize
        10 -> FieldStorageClass
        _ -> (error . showText) $ "Unknown index for InventoryOptionalField: " <> toText i
    fromEnum x = case x of
        FieldETag -> 0
        FieldEncryptionStatus -> 1
        FieldIntelligentTieringAccessTier -> 2
        FieldIsMultipartUploaded -> 3
        FieldLastModifiedDate -> 4
        FieldObjectLockLegalHoldStatus -> 5
        FieldObjectLockMode -> 6
        FieldObjectLockRetainUntilDate -> 7
        FieldReplicationStatus -> 8
        FieldSize -> 9
        FieldStorageClass -> 10
        InventoryOptionalField' name -> (error . showText) $ "Unknown InventoryOptionalField: " <> original name

-- | Represents the bounds of /known/ $InventoryOptionalField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InventoryOptionalField where
    minBound = FieldETag
    maxBound = FieldStorageClass

instance Hashable     InventoryOptionalField
instance NFData       InventoryOptionalField
instance ToByteString InventoryOptionalField
instance ToQuery      InventoryOptionalField
instance ToHeader     InventoryOptionalField

instance FromXML InventoryOptionalField where
    parseXML = parseXMLText "InventoryOptionalField"

instance ToXML InventoryOptionalField where
    toXML = toXMLText
