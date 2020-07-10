{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.State
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.State (
  State (
    ..
    , SAvailable
    , SDeleted
    , SDeleting
    , SExpired
    , SFailed
    , SPending
    , SPendingAcceptance
    , SRejected
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data State = State' (CI Text)
               deriving (Eq, Ord, Read, Show, Data, Typeable,
                         Generic)

pattern SAvailable :: State
pattern SAvailable = State' "Available"

pattern SDeleted :: State
pattern SDeleted = State' "Deleted"

pattern SDeleting :: State
pattern SDeleting = State' "Deleting"

pattern SExpired :: State
pattern SExpired = State' "Expired"

pattern SFailed :: State
pattern SFailed = State' "Failed"

pattern SPending :: State
pattern SPending = State' "Pending"

pattern SPendingAcceptance :: State
pattern SPendingAcceptance = State' "PendingAcceptance"

pattern SRejected :: State
pattern SRejected = State' "Rejected"

{-# COMPLETE
  SAvailable,
  SDeleted,
  SDeleting,
  SExpired,
  SFailed,
  SPending,
  SPendingAcceptance,
  SRejected,
  State' #-}

instance FromText State where
    parser = (State' . mk) <$> takeText

instance ToText State where
    toText (State' ci) = original ci

-- | Represents an enum of /known/ $State.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum State where
    toEnum i = case i of
        0 -> SAvailable
        1 -> SDeleted
        2 -> SDeleting
        3 -> SExpired
        4 -> SFailed
        5 -> SPending
        6 -> SPendingAcceptance
        7 -> SRejected
        _ -> (error . showText) $ "Unknown index for State: " <> toText i
    fromEnum x = case x of
        SAvailable -> 0
        SDeleted -> 1
        SDeleting -> 2
        SExpired -> 3
        SFailed -> 4
        SPending -> 5
        SPendingAcceptance -> 6
        SRejected -> 7
        State' name -> (error . showText) $ "Unknown State: " <> original name

-- | Represents the bounds of /known/ $State.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded State where
    minBound = SAvailable
    maxBound = SRejected

instance Hashable     State
instance NFData       State
instance ToByteString State
instance ToQuery      State
instance ToHeader     State

instance FromXML State where
    parseXML = parseXMLText "State"
