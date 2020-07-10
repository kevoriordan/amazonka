{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode (
  H264UnregisteredSeiTimecode (
    ..
    , HDisabled
    , HEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
data H264UnregisteredSeiTimecode = H264UnregisteredSeiTimecode' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern HDisabled :: H264UnregisteredSeiTimecode
pattern HDisabled = H264UnregisteredSeiTimecode' "DISABLED"

pattern HEnabled :: H264UnregisteredSeiTimecode
pattern HEnabled = H264UnregisteredSeiTimecode' "ENABLED"

{-# COMPLETE
  HDisabled,
  HEnabled,
  H264UnregisteredSeiTimecode' #-}

instance FromText H264UnregisteredSeiTimecode where
    parser = (H264UnregisteredSeiTimecode' . mk) <$> takeText

instance ToText H264UnregisteredSeiTimecode where
    toText (H264UnregisteredSeiTimecode' ci) = original ci

-- | Represents an enum of /known/ $H264UnregisteredSeiTimecode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264UnregisteredSeiTimecode where
    toEnum i = case i of
        0 -> HDisabled
        1 -> HEnabled
        _ -> (error . showText) $ "Unknown index for H264UnregisteredSeiTimecode: " <> toText i
    fromEnum x = case x of
        HDisabled -> 0
        HEnabled -> 1
        H264UnregisteredSeiTimecode' name -> (error . showText) $ "Unknown H264UnregisteredSeiTimecode: " <> original name

-- | Represents the bounds of /known/ $H264UnregisteredSeiTimecode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264UnregisteredSeiTimecode where
    minBound = HDisabled
    maxBound = HEnabled

instance Hashable     H264UnregisteredSeiTimecode
instance NFData       H264UnregisteredSeiTimecode
instance ToByteString H264UnregisteredSeiTimecode
instance ToQuery      H264UnregisteredSeiTimecode
instance ToHeader     H264UnregisteredSeiTimecode

instance ToJSON H264UnregisteredSeiTimecode where
    toJSON = toJSONText

instance FromJSON H264UnregisteredSeiTimecode where
    parseJSON = parseJSONText "H264UnregisteredSeiTimecode"
