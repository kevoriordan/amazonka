{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MFADeleteStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.MFADeleteStatus (
  MFADeleteStatus (
    ..
    , MDSDisabled
    , MDSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data MFADeleteStatus = MFADeleteStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern MDSDisabled :: MFADeleteStatus
pattern MDSDisabled = MFADeleteStatus' "Disabled"

pattern MDSEnabled :: MFADeleteStatus
pattern MDSEnabled = MFADeleteStatus' "Enabled"

{-# COMPLETE
  MDSDisabled,
  MDSEnabled,
  MFADeleteStatus' #-}

instance FromText MFADeleteStatus where
    parser = (MFADeleteStatus' . mk) <$> takeText

instance ToText MFADeleteStatus where
    toText (MFADeleteStatus' ci) = original ci

-- | Represents an enum of /known/ $MFADeleteStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MFADeleteStatus where
    toEnum i = case i of
        0 -> MDSDisabled
        1 -> MDSEnabled
        _ -> (error . showText) $ "Unknown index for MFADeleteStatus: " <> toText i
    fromEnum x = case x of
        MDSDisabled -> 0
        MDSEnabled -> 1
        MFADeleteStatus' name -> (error . showText) $ "Unknown MFADeleteStatus: " <> original name

-- | Represents the bounds of /known/ $MFADeleteStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MFADeleteStatus where
    minBound = MDSDisabled
    maxBound = MDSEnabled

instance Hashable     MFADeleteStatus
instance NFData       MFADeleteStatus
instance ToByteString MFADeleteStatus
instance ToQuery      MFADeleteStatus
instance ToHeader     MFADeleteStatus

instance FromXML MFADeleteStatus where
    parseXML = parseXMLText "MFADeleteStatus"
