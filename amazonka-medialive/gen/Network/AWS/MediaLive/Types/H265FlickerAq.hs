{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265FlickerAq
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265FlickerAq (
  H265FlickerAq (
    ..
    , HFADisabled
    , HFAEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H265 Flicker Aq
data H265FlickerAq = H265FlickerAq' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern HFADisabled :: H265FlickerAq
pattern HFADisabled = H265FlickerAq' "DISABLED"

pattern HFAEnabled :: H265FlickerAq
pattern HFAEnabled = H265FlickerAq' "ENABLED"

{-# COMPLETE
  HFADisabled,
  HFAEnabled,
  H265FlickerAq' #-}

instance FromText H265FlickerAq where
    parser = (H265FlickerAq' . mk) <$> takeText

instance ToText H265FlickerAq where
    toText (H265FlickerAq' ci) = original ci

-- | Represents an enum of /known/ $H265FlickerAq.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265FlickerAq where
    toEnum i = case i of
        0 -> HFADisabled
        1 -> HFAEnabled
        _ -> (error . showText) $ "Unknown index for H265FlickerAq: " <> toText i
    fromEnum x = case x of
        HFADisabled -> 0
        HFAEnabled -> 1
        H265FlickerAq' name -> (error . showText) $ "Unknown H265FlickerAq: " <> original name

-- | Represents the bounds of /known/ $H265FlickerAq.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265FlickerAq where
    minBound = HFADisabled
    maxBound = HFAEnabled

instance Hashable     H265FlickerAq
instance NFData       H265FlickerAq
instance ToByteString H265FlickerAq
instance ToQuery      H265FlickerAq
instance ToHeader     H265FlickerAq

instance ToJSON H265FlickerAq where
    toJSON = toJSONText

instance FromJSON H265FlickerAq where
    parseJSON = parseJSONText "H265FlickerAq"
