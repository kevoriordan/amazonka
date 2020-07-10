{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ReachabilityStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.ReachabilityStatus (
  ReachabilityStatus (
    ..
    , Done
    , Expired
    , Pending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReachabilityStatus = ReachabilityStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Done :: ReachabilityStatus
pattern Done = ReachabilityStatus' "DONE"

pattern Expired :: ReachabilityStatus
pattern Expired = ReachabilityStatus' "EXPIRED"

pattern Pending :: ReachabilityStatus
pattern Pending = ReachabilityStatus' "PENDING"

{-# COMPLETE
  Done,
  Expired,
  Pending,
  ReachabilityStatus' #-}

instance FromText ReachabilityStatus where
    parser = (ReachabilityStatus' . mk) <$> takeText

instance ToText ReachabilityStatus where
    toText (ReachabilityStatus' ci) = original ci

-- | Represents an enum of /known/ $ReachabilityStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReachabilityStatus where
    toEnum i = case i of
        0 -> Done
        1 -> Expired
        2 -> Pending
        _ -> (error . showText) $ "Unknown index for ReachabilityStatus: " <> toText i
    fromEnum x = case x of
        Done -> 0
        Expired -> 1
        Pending -> 2
        ReachabilityStatus' name -> (error . showText) $ "Unknown ReachabilityStatus: " <> original name

-- | Represents the bounds of /known/ $ReachabilityStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReachabilityStatus where
    minBound = Done
    maxBound = Pending

instance Hashable     ReachabilityStatus
instance NFData       ReachabilityStatus
instance ToByteString ReachabilityStatus
instance ToQuery      ReachabilityStatus
instance ToHeader     ReachabilityStatus

instance FromJSON ReachabilityStatus where
    parseJSON = parseJSONText "ReachabilityStatus"
