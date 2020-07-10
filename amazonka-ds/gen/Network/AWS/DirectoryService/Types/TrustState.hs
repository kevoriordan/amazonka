{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.TrustState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.TrustState (
  TrustState (
    ..
    , TSCreated
    , TSCreating
    , TSDeleted
    , TSDeleting
    , TSFailed
    , TSUpdateFailed
    , TSUpdated
    , TSUpdating
    , TSVerified
    , TSVerifyFailed
    , TSVerifying
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TrustState = TrustState' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern TSCreated :: TrustState
pattern TSCreated = TrustState' "Created"

pattern TSCreating :: TrustState
pattern TSCreating = TrustState' "Creating"

pattern TSDeleted :: TrustState
pattern TSDeleted = TrustState' "Deleted"

pattern TSDeleting :: TrustState
pattern TSDeleting = TrustState' "Deleting"

pattern TSFailed :: TrustState
pattern TSFailed = TrustState' "Failed"

pattern TSUpdateFailed :: TrustState
pattern TSUpdateFailed = TrustState' "UpdateFailed"

pattern TSUpdated :: TrustState
pattern TSUpdated = TrustState' "Updated"

pattern TSUpdating :: TrustState
pattern TSUpdating = TrustState' "Updating"

pattern TSVerified :: TrustState
pattern TSVerified = TrustState' "Verified"

pattern TSVerifyFailed :: TrustState
pattern TSVerifyFailed = TrustState' "VerifyFailed"

pattern TSVerifying :: TrustState
pattern TSVerifying = TrustState' "Verifying"

{-# COMPLETE
  TSCreated,
  TSCreating,
  TSDeleted,
  TSDeleting,
  TSFailed,
  TSUpdateFailed,
  TSUpdated,
  TSUpdating,
  TSVerified,
  TSVerifyFailed,
  TSVerifying,
  TrustState' #-}

instance FromText TrustState where
    parser = (TrustState' . mk) <$> takeText

instance ToText TrustState where
    toText (TrustState' ci) = original ci

-- | Represents an enum of /known/ $TrustState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrustState where
    toEnum i = case i of
        0 -> TSCreated
        1 -> TSCreating
        2 -> TSDeleted
        3 -> TSDeleting
        4 -> TSFailed
        5 -> TSUpdateFailed
        6 -> TSUpdated
        7 -> TSUpdating
        8 -> TSVerified
        9 -> TSVerifyFailed
        10 -> TSVerifying
        _ -> (error . showText) $ "Unknown index for TrustState: " <> toText i
    fromEnum x = case x of
        TSCreated -> 0
        TSCreating -> 1
        TSDeleted -> 2
        TSDeleting -> 3
        TSFailed -> 4
        TSUpdateFailed -> 5
        TSUpdated -> 6
        TSUpdating -> 7
        TSVerified -> 8
        TSVerifyFailed -> 9
        TSVerifying -> 10
        TrustState' name -> (error . showText) $ "Unknown TrustState: " <> original name

-- | Represents the bounds of /known/ $TrustState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrustState where
    minBound = TSCreated
    maxBound = TSVerifying

instance Hashable     TrustState
instance NFData       TrustState
instance ToByteString TrustState
instance ToQuery      TrustState
instance ToHeader     TrustState

instance FromJSON TrustState where
    parseJSON = parseJSONText "TrustState"
