{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus (
  PendingAutomaticFailoverStatus (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PendingAutomaticFailoverStatus = PendingAutomaticFailoverStatus' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern Disabled :: PendingAutomaticFailoverStatus
pattern Disabled = PendingAutomaticFailoverStatus' "disabled"

pattern Enabled :: PendingAutomaticFailoverStatus
pattern Enabled = PendingAutomaticFailoverStatus' "enabled"

{-# COMPLETE
  Disabled,
  Enabled,
  PendingAutomaticFailoverStatus' #-}

instance FromText PendingAutomaticFailoverStatus where
    parser = (PendingAutomaticFailoverStatus' . mk) <$> takeText

instance ToText PendingAutomaticFailoverStatus where
    toText (PendingAutomaticFailoverStatus' ci) = original ci

-- | Represents an enum of /known/ $PendingAutomaticFailoverStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PendingAutomaticFailoverStatus where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for PendingAutomaticFailoverStatus: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        PendingAutomaticFailoverStatus' name -> (error . showText) $ "Unknown PendingAutomaticFailoverStatus: " <> original name

-- | Represents the bounds of /known/ $PendingAutomaticFailoverStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PendingAutomaticFailoverStatus where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     PendingAutomaticFailoverStatus
instance NFData       PendingAutomaticFailoverStatus
instance ToByteString PendingAutomaticFailoverStatus
instance ToQuery      PendingAutomaticFailoverStatus
instance ToHeader     PendingAutomaticFailoverStatus

instance FromXML PendingAutomaticFailoverStatus where
    parseXML = parseXMLText "PendingAutomaticFailoverStatus"
