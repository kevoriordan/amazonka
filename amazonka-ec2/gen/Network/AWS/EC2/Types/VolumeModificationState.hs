{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeModificationState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeModificationState (
  VolumeModificationState (
    ..
    , Completed
    , Failed
    , Modifying
    , Optimizing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeModificationState = VolumeModificationState' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Completed :: VolumeModificationState
pattern Completed = VolumeModificationState' "completed"

pattern Failed :: VolumeModificationState
pattern Failed = VolumeModificationState' "failed"

pattern Modifying :: VolumeModificationState
pattern Modifying = VolumeModificationState' "modifying"

pattern Optimizing :: VolumeModificationState
pattern Optimizing = VolumeModificationState' "optimizing"

{-# COMPLETE
  Completed,
  Failed,
  Modifying,
  Optimizing,
  VolumeModificationState' #-}

instance FromText VolumeModificationState where
    parser = (VolumeModificationState' . mk) <$> takeText

instance ToText VolumeModificationState where
    toText (VolumeModificationState' ci) = original ci

-- | Represents an enum of /known/ $VolumeModificationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VolumeModificationState where
    toEnum i = case i of
        0 -> Completed
        1 -> Failed
        2 -> Modifying
        3 -> Optimizing
        _ -> (error . showText) $ "Unknown index for VolumeModificationState: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        Failed -> 1
        Modifying -> 2
        Optimizing -> 3
        VolumeModificationState' name -> (error . showText) $ "Unknown VolumeModificationState: " <> original name

-- | Represents the bounds of /known/ $VolumeModificationState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VolumeModificationState where
    minBound = Completed
    maxBound = Optimizing

instance Hashable     VolumeModificationState
instance NFData       VolumeModificationState
instance ToByteString VolumeModificationState
instance ToQuery      VolumeModificationState
instance ToHeader     VolumeModificationState

instance FromXML VolumeModificationState where
    parseXML = parseXMLText "VolumeModificationState"
