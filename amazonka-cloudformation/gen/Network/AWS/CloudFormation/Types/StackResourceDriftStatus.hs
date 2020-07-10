{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackResourceDriftStatus (
  StackResourceDriftStatus (
    ..
    , SRDSDeleted
    , SRDSInSync
    , SRDSModified
    , SRDSNotChecked
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackResourceDriftStatus = StackResourceDriftStatus' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern SRDSDeleted :: StackResourceDriftStatus
pattern SRDSDeleted = StackResourceDriftStatus' "DELETED"

pattern SRDSInSync :: StackResourceDriftStatus
pattern SRDSInSync = StackResourceDriftStatus' "IN_SYNC"

pattern SRDSModified :: StackResourceDriftStatus
pattern SRDSModified = StackResourceDriftStatus' "MODIFIED"

pattern SRDSNotChecked :: StackResourceDriftStatus
pattern SRDSNotChecked = StackResourceDriftStatus' "NOT_CHECKED"

{-# COMPLETE
  SRDSDeleted,
  SRDSInSync,
  SRDSModified,
  SRDSNotChecked,
  StackResourceDriftStatus' #-}

instance FromText StackResourceDriftStatus where
    parser = (StackResourceDriftStatus' . mk) <$> takeText

instance ToText StackResourceDriftStatus where
    toText (StackResourceDriftStatus' ci) = original ci

-- | Represents an enum of /known/ $StackResourceDriftStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackResourceDriftStatus where
    toEnum i = case i of
        0 -> SRDSDeleted
        1 -> SRDSInSync
        2 -> SRDSModified
        3 -> SRDSNotChecked
        _ -> (error . showText) $ "Unknown index for StackResourceDriftStatus: " <> toText i
    fromEnum x = case x of
        SRDSDeleted -> 0
        SRDSInSync -> 1
        SRDSModified -> 2
        SRDSNotChecked -> 3
        StackResourceDriftStatus' name -> (error . showText) $ "Unknown StackResourceDriftStatus: " <> original name

-- | Represents the bounds of /known/ $StackResourceDriftStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackResourceDriftStatus where
    minBound = SRDSDeleted
    maxBound = SRDSNotChecked

instance Hashable     StackResourceDriftStatus
instance NFData       StackResourceDriftStatus
instance ToByteString StackResourceDriftStatus
instance ToQuery      StackResourceDriftStatus
instance ToHeader     StackResourceDriftStatus

instance FromXML StackResourceDriftStatus where
    parseXML = parseXMLText "StackResourceDriftStatus"
