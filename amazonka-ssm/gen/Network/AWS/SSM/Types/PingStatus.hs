{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PingStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PingStatus (
  PingStatus (
    ..
    , ConnectionLost
    , Inactive
    , Online
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PingStatus = PingStatus' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ConnectionLost :: PingStatus
pattern ConnectionLost = PingStatus' "ConnectionLost"

pattern Inactive :: PingStatus
pattern Inactive = PingStatus' "Inactive"

pattern Online :: PingStatus
pattern Online = PingStatus' "Online"

{-# COMPLETE
  ConnectionLost,
  Inactive,
  Online,
  PingStatus' #-}

instance FromText PingStatus where
    parser = (PingStatus' . mk) <$> takeText

instance ToText PingStatus where
    toText (PingStatus' ci) = original ci

-- | Represents an enum of /known/ $PingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PingStatus where
    toEnum i = case i of
        0 -> ConnectionLost
        1 -> Inactive
        2 -> Online
        _ -> (error . showText) $ "Unknown index for PingStatus: " <> toText i
    fromEnum x = case x of
        ConnectionLost -> 0
        Inactive -> 1
        Online -> 2
        PingStatus' name -> (error . showText) $ "Unknown PingStatus: " <> original name

-- | Represents the bounds of /known/ $PingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PingStatus where
    minBound = ConnectionLost
    maxBound = Online

instance Hashable     PingStatus
instance NFData       PingStatus
instance ToByteString PingStatus
instance ToQuery      PingStatus
instance ToHeader     PingStatus

instance FromJSON PingStatus where
    parseJSON = parseJSONText "PingStatus"
