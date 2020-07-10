{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacRateControlMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AacRateControlMode (
  AacRateControlMode (
    ..
    , ARCMCbr
    , ARCMVbr
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Aac Rate Control Mode
data AacRateControlMode = AacRateControlMode' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern ARCMCbr :: AacRateControlMode
pattern ARCMCbr = AacRateControlMode' "CBR"

pattern ARCMVbr :: AacRateControlMode
pattern ARCMVbr = AacRateControlMode' "VBR"

{-# COMPLETE
  ARCMCbr,
  ARCMVbr,
  AacRateControlMode' #-}

instance FromText AacRateControlMode where
    parser = (AacRateControlMode' . mk) <$> takeText

instance ToText AacRateControlMode where
    toText (AacRateControlMode' ci) = original ci

-- | Represents an enum of /known/ $AacRateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AacRateControlMode where
    toEnum i = case i of
        0 -> ARCMCbr
        1 -> ARCMVbr
        _ -> (error . showText) $ "Unknown index for AacRateControlMode: " <> toText i
    fromEnum x = case x of
        ARCMCbr -> 0
        ARCMVbr -> 1
        AacRateControlMode' name -> (error . showText) $ "Unknown AacRateControlMode: " <> original name

-- | Represents the bounds of /known/ $AacRateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AacRateControlMode where
    minBound = ARCMCbr
    maxBound = ARCMVbr

instance Hashable     AacRateControlMode
instance NFData       AacRateControlMode
instance ToByteString AacRateControlMode
instance ToQuery      AacRateControlMode
instance ToHeader     AacRateControlMode

instance ToJSON AacRateControlMode where
    toJSON = toJSONText

instance FromJSON AacRateControlMode where
    parseJSON = parseJSONText "AacRateControlMode"
