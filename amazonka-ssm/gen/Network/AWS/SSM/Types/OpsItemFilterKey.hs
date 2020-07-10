{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OpsItemFilterKey (
  OpsItemFilterKey (
    ..
    , OIFKAutomationId
    , OIFKCategory
    , OIFKCreatedBy
    , OIFKCreatedTime
    , OIFKLastModifiedTime
    , OIFKOperationalData
    , OIFKOperationalDataKey
    , OIFKOperationalDataValue
    , OIFKOpsItemId
    , OIFKPriority
    , OIFKResourceId
    , OIFKSeverity
    , OIFKSource
    , OIFKStatus
    , OIFKTitle
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OpsItemFilterKey = OpsItemFilterKey' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern OIFKAutomationId :: OpsItemFilterKey
pattern OIFKAutomationId = OpsItemFilterKey' "AutomationId"

pattern OIFKCategory :: OpsItemFilterKey
pattern OIFKCategory = OpsItemFilterKey' "Category"

pattern OIFKCreatedBy :: OpsItemFilterKey
pattern OIFKCreatedBy = OpsItemFilterKey' "CreatedBy"

pattern OIFKCreatedTime :: OpsItemFilterKey
pattern OIFKCreatedTime = OpsItemFilterKey' "CreatedTime"

pattern OIFKLastModifiedTime :: OpsItemFilterKey
pattern OIFKLastModifiedTime = OpsItemFilterKey' "LastModifiedTime"

pattern OIFKOperationalData :: OpsItemFilterKey
pattern OIFKOperationalData = OpsItemFilterKey' "OperationalData"

pattern OIFKOperationalDataKey :: OpsItemFilterKey
pattern OIFKOperationalDataKey = OpsItemFilterKey' "OperationalDataKey"

pattern OIFKOperationalDataValue :: OpsItemFilterKey
pattern OIFKOperationalDataValue = OpsItemFilterKey' "OperationalDataValue"

pattern OIFKOpsItemId :: OpsItemFilterKey
pattern OIFKOpsItemId = OpsItemFilterKey' "OpsItemId"

pattern OIFKPriority :: OpsItemFilterKey
pattern OIFKPriority = OpsItemFilterKey' "Priority"

pattern OIFKResourceId :: OpsItemFilterKey
pattern OIFKResourceId = OpsItemFilterKey' "ResourceId"

pattern OIFKSeverity :: OpsItemFilterKey
pattern OIFKSeverity = OpsItemFilterKey' "Severity"

pattern OIFKSource :: OpsItemFilterKey
pattern OIFKSource = OpsItemFilterKey' "Source"

pattern OIFKStatus :: OpsItemFilterKey
pattern OIFKStatus = OpsItemFilterKey' "Status"

pattern OIFKTitle :: OpsItemFilterKey
pattern OIFKTitle = OpsItemFilterKey' "Title"

{-# COMPLETE
  OIFKAutomationId,
  OIFKCategory,
  OIFKCreatedBy,
  OIFKCreatedTime,
  OIFKLastModifiedTime,
  OIFKOperationalData,
  OIFKOperationalDataKey,
  OIFKOperationalDataValue,
  OIFKOpsItemId,
  OIFKPriority,
  OIFKResourceId,
  OIFKSeverity,
  OIFKSource,
  OIFKStatus,
  OIFKTitle,
  OpsItemFilterKey' #-}

instance FromText OpsItemFilterKey where
    parser = (OpsItemFilterKey' . mk) <$> takeText

instance ToText OpsItemFilterKey where
    toText (OpsItemFilterKey' ci) = original ci

-- | Represents an enum of /known/ $OpsItemFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OpsItemFilterKey where
    toEnum i = case i of
        0 -> OIFKAutomationId
        1 -> OIFKCategory
        2 -> OIFKCreatedBy
        3 -> OIFKCreatedTime
        4 -> OIFKLastModifiedTime
        5 -> OIFKOperationalData
        6 -> OIFKOperationalDataKey
        7 -> OIFKOperationalDataValue
        8 -> OIFKOpsItemId
        9 -> OIFKPriority
        10 -> OIFKResourceId
        11 -> OIFKSeverity
        12 -> OIFKSource
        13 -> OIFKStatus
        14 -> OIFKTitle
        _ -> (error . showText) $ "Unknown index for OpsItemFilterKey: " <> toText i
    fromEnum x = case x of
        OIFKAutomationId -> 0
        OIFKCategory -> 1
        OIFKCreatedBy -> 2
        OIFKCreatedTime -> 3
        OIFKLastModifiedTime -> 4
        OIFKOperationalData -> 5
        OIFKOperationalDataKey -> 6
        OIFKOperationalDataValue -> 7
        OIFKOpsItemId -> 8
        OIFKPriority -> 9
        OIFKResourceId -> 10
        OIFKSeverity -> 11
        OIFKSource -> 12
        OIFKStatus -> 13
        OIFKTitle -> 14
        OpsItemFilterKey' name -> (error . showText) $ "Unknown OpsItemFilterKey: " <> original name

-- | Represents the bounds of /known/ $OpsItemFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OpsItemFilterKey where
    minBound = OIFKAutomationId
    maxBound = OIFKTitle

instance Hashable     OpsItemFilterKey
instance NFData       OpsItemFilterKey
instance ToByteString OpsItemFilterKey
instance ToQuery      OpsItemFilterKey
instance ToHeader     OpsItemFilterKey

instance ToJSON OpsItemFilterKey where
    toJSON = toJSONText
