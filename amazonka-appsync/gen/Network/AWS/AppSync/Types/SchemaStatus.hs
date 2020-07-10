{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.SchemaStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.SchemaStatus (
  SchemaStatus (
    ..
    , SSActive
    , SSDeleting
    , SSFailed
    , SSNotApplicable
    , SSProcessing
    , SSSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SchemaStatus = SchemaStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern SSActive :: SchemaStatus
pattern SSActive = SchemaStatus' "ACTIVE"

pattern SSDeleting :: SchemaStatus
pattern SSDeleting = SchemaStatus' "DELETING"

pattern SSFailed :: SchemaStatus
pattern SSFailed = SchemaStatus' "FAILED"

pattern SSNotApplicable :: SchemaStatus
pattern SSNotApplicable = SchemaStatus' "NOT_APPLICABLE"

pattern SSProcessing :: SchemaStatus
pattern SSProcessing = SchemaStatus' "PROCESSING"

pattern SSSuccess :: SchemaStatus
pattern SSSuccess = SchemaStatus' "SUCCESS"

{-# COMPLETE
  SSActive,
  SSDeleting,
  SSFailed,
  SSNotApplicable,
  SSProcessing,
  SSSuccess,
  SchemaStatus' #-}

instance FromText SchemaStatus where
    parser = (SchemaStatus' . mk) <$> takeText

instance ToText SchemaStatus where
    toText (SchemaStatus' ci) = original ci

-- | Represents an enum of /known/ $SchemaStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SchemaStatus where
    toEnum i = case i of
        0 -> SSActive
        1 -> SSDeleting
        2 -> SSFailed
        3 -> SSNotApplicable
        4 -> SSProcessing
        5 -> SSSuccess
        _ -> (error . showText) $ "Unknown index for SchemaStatus: " <> toText i
    fromEnum x = case x of
        SSActive -> 0
        SSDeleting -> 1
        SSFailed -> 2
        SSNotApplicable -> 3
        SSProcessing -> 4
        SSSuccess -> 5
        SchemaStatus' name -> (error . showText) $ "Unknown SchemaStatus: " <> original name

-- | Represents the bounds of /known/ $SchemaStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SchemaStatus where
    minBound = SSActive
    maxBound = SSSuccess

instance Hashable     SchemaStatus
instance NFData       SchemaStatus
instance ToByteString SchemaStatus
instance ToQuery      SchemaStatus
instance ToHeader     SchemaStatus

instance FromJSON SchemaStatus where
    parseJSON = parseJSONText "SchemaStatus"
