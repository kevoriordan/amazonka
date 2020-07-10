{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeState (
  VolumeState (
    ..
    , VAvailable
    , VCreating
    , VDeleted
    , VDeleting
    , VError'
    , VInUse
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeState = VolumeState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern VAvailable :: VolumeState
pattern VAvailable = VolumeState' "available"

pattern VCreating :: VolumeState
pattern VCreating = VolumeState' "creating"

pattern VDeleted :: VolumeState
pattern VDeleted = VolumeState' "deleted"

pattern VDeleting :: VolumeState
pattern VDeleting = VolumeState' "deleting"

pattern VError' :: VolumeState
pattern VError' = VolumeState' "error"

pattern VInUse :: VolumeState
pattern VInUse = VolumeState' "in-use"

{-# COMPLETE
  VAvailable,
  VCreating,
  VDeleted,
  VDeleting,
  VError',
  VInUse,
  VolumeState' #-}

instance FromText VolumeState where
    parser = (VolumeState' . mk) <$> takeText

instance ToText VolumeState where
    toText (VolumeState' ci) = original ci

-- | Represents an enum of /known/ $VolumeState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VolumeState where
    toEnum i = case i of
        0 -> VAvailable
        1 -> VCreating
        2 -> VDeleted
        3 -> VDeleting
        4 -> VError'
        5 -> VInUse
        _ -> (error . showText) $ "Unknown index for VolumeState: " <> toText i
    fromEnum x = case x of
        VAvailable -> 0
        VCreating -> 1
        VDeleted -> 2
        VDeleting -> 3
        VError' -> 4
        VInUse -> 5
        VolumeState' name -> (error . showText) $ "Unknown VolumeState: " <> original name

-- | Represents the bounds of /known/ $VolumeState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VolumeState where
    minBound = VAvailable
    maxBound = VInUse

instance Hashable     VolumeState
instance NFData       VolumeState
instance ToByteString VolumeState
instance ToQuery      VolumeState
instance ToHeader     VolumeState

instance FromXML VolumeState where
    parseXML = parseXMLText "VolumeState"
