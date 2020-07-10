{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.F4vMoovPlacement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.F4vMoovPlacement (
  F4vMoovPlacement (
    ..
    , FMPNormal
    , FMPProgressiveDownload
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
data F4vMoovPlacement = F4vMoovPlacement' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern FMPNormal :: F4vMoovPlacement
pattern FMPNormal = F4vMoovPlacement' "NORMAL"

pattern FMPProgressiveDownload :: F4vMoovPlacement
pattern FMPProgressiveDownload = F4vMoovPlacement' "PROGRESSIVE_DOWNLOAD"

{-# COMPLETE
  FMPNormal,
  FMPProgressiveDownload,
  F4vMoovPlacement' #-}

instance FromText F4vMoovPlacement where
    parser = (F4vMoovPlacement' . mk) <$> takeText

instance ToText F4vMoovPlacement where
    toText (F4vMoovPlacement' ci) = original ci

-- | Represents an enum of /known/ $F4vMoovPlacement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum F4vMoovPlacement where
    toEnum i = case i of
        0 -> FMPNormal
        1 -> FMPProgressiveDownload
        _ -> (error . showText) $ "Unknown index for F4vMoovPlacement: " <> toText i
    fromEnum x = case x of
        FMPNormal -> 0
        FMPProgressiveDownload -> 1
        F4vMoovPlacement' name -> (error . showText) $ "Unknown F4vMoovPlacement: " <> original name

-- | Represents the bounds of /known/ $F4vMoovPlacement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded F4vMoovPlacement where
    minBound = FMPNormal
    maxBound = FMPProgressiveDownload

instance Hashable     F4vMoovPlacement
instance NFData       F4vMoovPlacement
instance ToByteString F4vMoovPlacement
instance ToQuery      F4vMoovPlacement
instance ToHeader     F4vMoovPlacement

instance ToJSON F4vMoovPlacement where
    toJSON = toJSONText

instance FromJSON F4vMoovPlacement where
    parseJSON = parseJSONText "F4vMoovPlacement"
