{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264FlickerAq
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264FlickerAq (
  H264FlickerAq (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Flicker Aq
data H264FlickerAq = H264FlickerAq' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Disabled :: H264FlickerAq
pattern Disabled = H264FlickerAq' "DISABLED"

pattern Enabled :: H264FlickerAq
pattern Enabled = H264FlickerAq' "ENABLED"

{-# COMPLETE
  Disabled,
  Enabled,
  H264FlickerAq' #-}

instance FromText H264FlickerAq where
    parser = (H264FlickerAq' . mk) <$> takeText

instance ToText H264FlickerAq where
    toText (H264FlickerAq' ci) = original ci

-- | Represents an enum of /known/ $H264FlickerAq.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264FlickerAq where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for H264FlickerAq: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        H264FlickerAq' name -> (error . showText) $ "Unknown H264FlickerAq: " <> original name

-- | Represents the bounds of /known/ $H264FlickerAq.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264FlickerAq where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     H264FlickerAq
instance NFData       H264FlickerAq
instance ToByteString H264FlickerAq
instance ToQuery      H264FlickerAq
instance ToHeader     H264FlickerAq

instance ToJSON H264FlickerAq where
    toJSON = toJSONText

instance FromJSON H264FlickerAq where
    parseJSON = parseJSONText "H264FlickerAq"
