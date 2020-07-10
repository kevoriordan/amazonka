{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.ChannelStatus (
  ChannelStatus (
    ..
    , CSActive
    , CSCreating
    , CSDeleting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChannelStatus = ChannelStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern CSActive :: ChannelStatus
pattern CSActive = ChannelStatus' "ACTIVE"

pattern CSCreating :: ChannelStatus
pattern CSCreating = ChannelStatus' "CREATING"

pattern CSDeleting :: ChannelStatus
pattern CSDeleting = ChannelStatus' "DELETING"

{-# COMPLETE
  CSActive,
  CSCreating,
  CSDeleting,
  ChannelStatus' #-}

instance FromText ChannelStatus where
    parser = (ChannelStatus' . mk) <$> takeText

instance ToText ChannelStatus where
    toText (ChannelStatus' ci) = original ci

-- | Represents an enum of /known/ $ChannelStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChannelStatus where
    toEnum i = case i of
        0 -> CSActive
        1 -> CSCreating
        2 -> CSDeleting
        _ -> (error . showText) $ "Unknown index for ChannelStatus: " <> toText i
    fromEnum x = case x of
        CSActive -> 0
        CSCreating -> 1
        CSDeleting -> 2
        ChannelStatus' name -> (error . showText) $ "Unknown ChannelStatus: " <> original name

-- | Represents the bounds of /known/ $ChannelStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChannelStatus where
    minBound = CSActive
    maxBound = CSDeleting

instance Hashable     ChannelStatus
instance NFData       ChannelStatus
instance ToByteString ChannelStatus
instance ToQuery      ChannelStatus
instance ToHeader     ChannelStatus

instance FromJSON ChannelStatus where
    parseJSON = parseJSONText "ChannelStatus"
