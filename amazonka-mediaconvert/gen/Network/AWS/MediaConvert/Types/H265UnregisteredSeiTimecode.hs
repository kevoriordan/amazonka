{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode (
  H265UnregisteredSeiTimecode (
    ..
    , HUSTDisabled
    , HUSTEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
data H265UnregisteredSeiTimecode = H265UnregisteredSeiTimecode' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern HUSTDisabled :: H265UnregisteredSeiTimecode
pattern HUSTDisabled = H265UnregisteredSeiTimecode' "DISABLED"

pattern HUSTEnabled :: H265UnregisteredSeiTimecode
pattern HUSTEnabled = H265UnregisteredSeiTimecode' "ENABLED"

{-# COMPLETE
  HUSTDisabled,
  HUSTEnabled,
  H265UnregisteredSeiTimecode' #-}

instance FromText H265UnregisteredSeiTimecode where
    parser = (H265UnregisteredSeiTimecode' . mk) <$> takeText

instance ToText H265UnregisteredSeiTimecode where
    toText (H265UnregisteredSeiTimecode' ci) = original ci

-- | Represents an enum of /known/ $H265UnregisteredSeiTimecode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265UnregisteredSeiTimecode where
    toEnum i = case i of
        0 -> HUSTDisabled
        1 -> HUSTEnabled
        _ -> (error . showText) $ "Unknown index for H265UnregisteredSeiTimecode: " <> toText i
    fromEnum x = case x of
        HUSTDisabled -> 0
        HUSTEnabled -> 1
        H265UnregisteredSeiTimecode' name -> (error . showText) $ "Unknown H265UnregisteredSeiTimecode: " <> original name

-- | Represents the bounds of /known/ $H265UnregisteredSeiTimecode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265UnregisteredSeiTimecode where
    minBound = HUSTDisabled
    maxBound = HUSTEnabled

instance Hashable     H265UnregisteredSeiTimecode
instance NFData       H265UnregisteredSeiTimecode
instance ToByteString H265UnregisteredSeiTimecode
instance ToQuery      H265UnregisteredSeiTimecode
instance ToHeader     H265UnregisteredSeiTimecode

instance ToJSON H265UnregisteredSeiTimecode where
    toJSON = toJSONText

instance FromJSON H265UnregisteredSeiTimecode where
    parseJSON = parseJSONText "H265UnregisteredSeiTimecode"
