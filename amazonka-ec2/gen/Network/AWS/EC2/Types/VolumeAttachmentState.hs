{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeAttachmentState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeAttachmentState (
  VolumeAttachmentState (
    ..
    , VAttached
    , VAttaching
    , VBusy
    , VDetached
    , VDetaching
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeAttachmentState = VolumeAttachmentState' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern VAttached :: VolumeAttachmentState
pattern VAttached = VolumeAttachmentState' "attached"

pattern VAttaching :: VolumeAttachmentState
pattern VAttaching = VolumeAttachmentState' "attaching"

pattern VBusy :: VolumeAttachmentState
pattern VBusy = VolumeAttachmentState' "busy"

pattern VDetached :: VolumeAttachmentState
pattern VDetached = VolumeAttachmentState' "detached"

pattern VDetaching :: VolumeAttachmentState
pattern VDetaching = VolumeAttachmentState' "detaching"

{-# COMPLETE
  VAttached,
  VAttaching,
  VBusy,
  VDetached,
  VDetaching,
  VolumeAttachmentState' #-}

instance FromText VolumeAttachmentState where
    parser = (VolumeAttachmentState' . mk) <$> takeText

instance ToText VolumeAttachmentState where
    toText (VolumeAttachmentState' ci) = original ci

-- | Represents an enum of /known/ $VolumeAttachmentState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VolumeAttachmentState where
    toEnum i = case i of
        0 -> VAttached
        1 -> VAttaching
        2 -> VBusy
        3 -> VDetached
        4 -> VDetaching
        _ -> (error . showText) $ "Unknown index for VolumeAttachmentState: " <> toText i
    fromEnum x = case x of
        VAttached -> 0
        VAttaching -> 1
        VBusy -> 2
        VDetached -> 3
        VDetaching -> 4
        VolumeAttachmentState' name -> (error . showText) $ "Unknown VolumeAttachmentState: " <> original name

-- | Represents the bounds of /known/ $VolumeAttachmentState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VolumeAttachmentState where
    minBound = VAttached
    maxBound = VDetaching

instance Hashable     VolumeAttachmentState
instance NFData       VolumeAttachmentState
instance ToByteString VolumeAttachmentState
instance ToQuery      VolumeAttachmentState
instance ToHeader     VolumeAttachmentState

instance FromXML VolumeAttachmentState where
    parseXML = parseXMLText "VolumeAttachmentState"
