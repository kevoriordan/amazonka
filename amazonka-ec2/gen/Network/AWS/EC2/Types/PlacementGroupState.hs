{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementGroupState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PlacementGroupState (
  PlacementGroupState (
    ..
    , Available
    , Deleted
    , Deleting
    , Pending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PlacementGroupState = PlacementGroupState' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Available :: PlacementGroupState
pattern Available = PlacementGroupState' "available"

pattern Deleted :: PlacementGroupState
pattern Deleted = PlacementGroupState' "deleted"

pattern Deleting :: PlacementGroupState
pattern Deleting = PlacementGroupState' "deleting"

pattern Pending :: PlacementGroupState
pattern Pending = PlacementGroupState' "pending"

{-# COMPLETE
  Available,
  Deleted,
  Deleting,
  Pending,
  PlacementGroupState' #-}

instance FromText PlacementGroupState where
    parser = (PlacementGroupState' . mk) <$> takeText

instance ToText PlacementGroupState where
    toText (PlacementGroupState' ci) = original ci

-- | Represents an enum of /known/ $PlacementGroupState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PlacementGroupState where
    toEnum i = case i of
        0 -> Available
        1 -> Deleted
        2 -> Deleting
        3 -> Pending
        _ -> (error . showText) $ "Unknown index for PlacementGroupState: " <> toText i
    fromEnum x = case x of
        Available -> 0
        Deleted -> 1
        Deleting -> 2
        Pending -> 3
        PlacementGroupState' name -> (error . showText) $ "Unknown PlacementGroupState: " <> original name

-- | Represents the bounds of /known/ $PlacementGroupState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PlacementGroupState where
    minBound = Available
    maxBound = Pending

instance Hashable     PlacementGroupState
instance NFData       PlacementGroupState
instance ToByteString PlacementGroupState
instance ToQuery      PlacementGroupState
instance ToHeader     PlacementGroupState

instance FromXML PlacementGroupState where
    parseXML = parseXMLText "PlacementGroupState"
