{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType (
  CloudFormationStackRecordSourceType (
    ..
    , ExportSnapshotRecord
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CloudFormationStackRecordSourceType = CloudFormationStackRecordSourceType' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern ExportSnapshotRecord :: CloudFormationStackRecordSourceType
pattern ExportSnapshotRecord = CloudFormationStackRecordSourceType' "ExportSnapshotRecord"

{-# COMPLETE
  ExportSnapshotRecord,
  CloudFormationStackRecordSourceType' #-}

instance FromText CloudFormationStackRecordSourceType where
    parser = (CloudFormationStackRecordSourceType' . mk) <$> takeText

instance ToText CloudFormationStackRecordSourceType where
    toText (CloudFormationStackRecordSourceType' ci) = original ci

-- | Represents an enum of /known/ $CloudFormationStackRecordSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CloudFormationStackRecordSourceType where
    toEnum i = case i of
        0 -> ExportSnapshotRecord
        _ -> (error . showText) $ "Unknown index for CloudFormationStackRecordSourceType: " <> toText i
    fromEnum x = case x of
        ExportSnapshotRecord -> 0
        CloudFormationStackRecordSourceType' name -> (error . showText) $ "Unknown CloudFormationStackRecordSourceType: " <> original name

-- | Represents the bounds of /known/ $CloudFormationStackRecordSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CloudFormationStackRecordSourceType where
    minBound = ExportSnapshotRecord
    maxBound = ExportSnapshotRecord

instance Hashable     CloudFormationStackRecordSourceType
instance NFData       CloudFormationStackRecordSourceType
instance ToByteString CloudFormationStackRecordSourceType
instance ToQuery      CloudFormationStackRecordSourceType
instance ToHeader     CloudFormationStackRecordSourceType

instance FromJSON CloudFormationStackRecordSourceType where
    parseJSON = parseJSONText "CloudFormationStackRecordSourceType"
