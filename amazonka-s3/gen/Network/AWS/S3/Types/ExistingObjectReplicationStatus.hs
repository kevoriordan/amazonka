{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ExistingObjectReplicationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ExistingObjectReplicationStatus (
  ExistingObjectReplicationStatus (
    ..
    , EORSDisabled
    , EORSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ExistingObjectReplicationStatus = ExistingObjectReplicationStatus' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern EORSDisabled :: ExistingObjectReplicationStatus
pattern EORSDisabled = ExistingObjectReplicationStatus' "Disabled"

pattern EORSEnabled :: ExistingObjectReplicationStatus
pattern EORSEnabled = ExistingObjectReplicationStatus' "Enabled"

{-# COMPLETE
  EORSDisabled,
  EORSEnabled,
  ExistingObjectReplicationStatus' #-}

instance FromText ExistingObjectReplicationStatus where
    parser = (ExistingObjectReplicationStatus' . mk) <$> takeText

instance ToText ExistingObjectReplicationStatus where
    toText (ExistingObjectReplicationStatus' ci) = original ci

-- | Represents an enum of /known/ $ExistingObjectReplicationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExistingObjectReplicationStatus where
    toEnum i = case i of
        0 -> EORSDisabled
        1 -> EORSEnabled
        _ -> (error . showText) $ "Unknown index for ExistingObjectReplicationStatus: " <> toText i
    fromEnum x = case x of
        EORSDisabled -> 0
        EORSEnabled -> 1
        ExistingObjectReplicationStatus' name -> (error . showText) $ "Unknown ExistingObjectReplicationStatus: " <> original name

-- | Represents the bounds of /known/ $ExistingObjectReplicationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExistingObjectReplicationStatus where
    minBound = EORSDisabled
    maxBound = EORSEnabled

instance Hashable     ExistingObjectReplicationStatus
instance NFData       ExistingObjectReplicationStatus
instance ToByteString ExistingObjectReplicationStatus
instance ToQuery      ExistingObjectReplicationStatus
instance ToHeader     ExistingObjectReplicationStatus

instance FromXML ExistingObjectReplicationStatus where
    parseXML = parseXMLText "ExistingObjectReplicationStatus"

instance ToXML ExistingObjectReplicationStatus where
    toXML = toXMLText
