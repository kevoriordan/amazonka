{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType (
  ExportSnapshotRecordSourceType (
    ..
    , DiskSnapshot
    , InstanceSnapshot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExportSnapshotRecordSourceType = ExportSnapshotRecordSourceType' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern DiskSnapshot :: ExportSnapshotRecordSourceType
pattern DiskSnapshot = ExportSnapshotRecordSourceType' "DiskSnapshot"

pattern InstanceSnapshot :: ExportSnapshotRecordSourceType
pattern InstanceSnapshot = ExportSnapshotRecordSourceType' "InstanceSnapshot"

{-# COMPLETE
  DiskSnapshot,
  InstanceSnapshot,
  ExportSnapshotRecordSourceType' #-}

instance FromText ExportSnapshotRecordSourceType where
    parser = (ExportSnapshotRecordSourceType' . mk) <$> takeText

instance ToText ExportSnapshotRecordSourceType where
    toText (ExportSnapshotRecordSourceType' ci) = original ci

-- | Represents an enum of /known/ $ExportSnapshotRecordSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExportSnapshotRecordSourceType where
    toEnum i = case i of
        0 -> DiskSnapshot
        1 -> InstanceSnapshot
        _ -> (error . showText) $ "Unknown index for ExportSnapshotRecordSourceType: " <> toText i
    fromEnum x = case x of
        DiskSnapshot -> 0
        InstanceSnapshot -> 1
        ExportSnapshotRecordSourceType' name -> (error . showText) $ "Unknown ExportSnapshotRecordSourceType: " <> original name

-- | Represents the bounds of /known/ $ExportSnapshotRecordSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExportSnapshotRecordSourceType where
    minBound = DiskSnapshot
    maxBound = InstanceSnapshot

instance Hashable     ExportSnapshotRecordSourceType
instance NFData       ExportSnapshotRecordSourceType
instance ToByteString ExportSnapshotRecordSourceType
instance ToQuery      ExportSnapshotRecordSourceType
instance ToHeader     ExportSnapshotRecordSourceType

instance FromJSON ExportSnapshotRecordSourceType where
    parseJSON = parseJSONText "ExportSnapshotRecordSourceType"
