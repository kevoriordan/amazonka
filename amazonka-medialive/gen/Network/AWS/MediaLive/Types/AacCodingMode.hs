{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacCodingMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AacCodingMode (
  AacCodingMode (
    ..
    , ACMAdReceiverMix
    , ACMCodingMode10
    , ACMCodingMode11
    , ACMCodingMode20
    , ACMCodingMode51
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Aac Coding Mode
data AacCodingMode = AacCodingMode' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern ACMAdReceiverMix :: AacCodingMode
pattern ACMAdReceiverMix = AacCodingMode' "AD_RECEIVER_MIX"

pattern ACMCodingMode10 :: AacCodingMode
pattern ACMCodingMode10 = AacCodingMode' "CODING_MODE_1_0"

pattern ACMCodingMode11 :: AacCodingMode
pattern ACMCodingMode11 = AacCodingMode' "CODING_MODE_1_1"

pattern ACMCodingMode20 :: AacCodingMode
pattern ACMCodingMode20 = AacCodingMode' "CODING_MODE_2_0"

pattern ACMCodingMode51 :: AacCodingMode
pattern ACMCodingMode51 = AacCodingMode' "CODING_MODE_5_1"

{-# COMPLETE
  ACMAdReceiverMix,
  ACMCodingMode10,
  ACMCodingMode11,
  ACMCodingMode20,
  ACMCodingMode51,
  AacCodingMode' #-}

instance FromText AacCodingMode where
    parser = (AacCodingMode' . mk) <$> takeText

instance ToText AacCodingMode where
    toText (AacCodingMode' ci) = original ci

-- | Represents an enum of /known/ $AacCodingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AacCodingMode where
    toEnum i = case i of
        0 -> ACMAdReceiverMix
        1 -> ACMCodingMode10
        2 -> ACMCodingMode11
        3 -> ACMCodingMode20
        4 -> ACMCodingMode51
        _ -> (error . showText) $ "Unknown index for AacCodingMode: " <> toText i
    fromEnum x = case x of
        ACMAdReceiverMix -> 0
        ACMCodingMode10 -> 1
        ACMCodingMode11 -> 2
        ACMCodingMode20 -> 3
        ACMCodingMode51 -> 4
        AacCodingMode' name -> (error . showText) $ "Unknown AacCodingMode: " <> original name

-- | Represents the bounds of /known/ $AacCodingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AacCodingMode where
    minBound = ACMAdReceiverMix
    maxBound = ACMCodingMode51

instance Hashable     AacCodingMode
instance NFData       AacCodingMode
instance ToByteString AacCodingMode
instance ToQuery      AacCodingMode
instance ToHeader     AacCodingMode

instance ToJSON AacCodingMode where
    toJSON = toJSONText

instance FromJSON AacCodingMode where
    parseJSON = parseJSONText "AacCodingMode"
