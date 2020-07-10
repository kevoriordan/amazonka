{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SubscriptionState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.SubscriptionState (
  SubscriptionState (
    ..
    , Active
    , Inactive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SubscriptionState = SubscriptionState' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Active :: SubscriptionState
pattern Active = SubscriptionState' "ACTIVE"

pattern Inactive :: SubscriptionState
pattern Inactive = SubscriptionState' "INACTIVE"

{-# COMPLETE
  Active,
  Inactive,
  SubscriptionState' #-}

instance FromText SubscriptionState where
    parser = (SubscriptionState' . mk) <$> takeText

instance ToText SubscriptionState where
    toText (SubscriptionState' ci) = original ci

-- | Represents an enum of /known/ $SubscriptionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SubscriptionState where
    toEnum i = case i of
        0 -> Active
        1 -> Inactive
        _ -> (error . showText) $ "Unknown index for SubscriptionState: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Inactive -> 1
        SubscriptionState' name -> (error . showText) $ "Unknown SubscriptionState: " <> original name

-- | Represents the bounds of /known/ $SubscriptionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SubscriptionState where
    minBound = Active
    maxBound = Inactive

instance Hashable     SubscriptionState
instance NFData       SubscriptionState
instance ToByteString SubscriptionState
instance ToQuery      SubscriptionState
instance ToHeader     SubscriptionState

instance FromJSON SubscriptionState where
    parseJSON = parseJSONText "SubscriptionState"
