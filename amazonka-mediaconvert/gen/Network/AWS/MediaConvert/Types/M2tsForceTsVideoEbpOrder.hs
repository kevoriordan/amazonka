{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder (
  M2tsForceTsVideoEbpOrder (
    ..
    , MFTVEODefault
    , MFTVEOForce
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
data M2tsForceTsVideoEbpOrder = M2tsForceTsVideoEbpOrder' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern MFTVEODefault :: M2tsForceTsVideoEbpOrder
pattern MFTVEODefault = M2tsForceTsVideoEbpOrder' "DEFAULT"

pattern MFTVEOForce :: M2tsForceTsVideoEbpOrder
pattern MFTVEOForce = M2tsForceTsVideoEbpOrder' "FORCE"

{-# COMPLETE
  MFTVEODefault,
  MFTVEOForce,
  M2tsForceTsVideoEbpOrder' #-}

instance FromText M2tsForceTsVideoEbpOrder where
    parser = (M2tsForceTsVideoEbpOrder' . mk) <$> takeText

instance ToText M2tsForceTsVideoEbpOrder where
    toText (M2tsForceTsVideoEbpOrder' ci) = original ci

-- | Represents an enum of /known/ $M2tsForceTsVideoEbpOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsForceTsVideoEbpOrder where
    toEnum i = case i of
        0 -> MFTVEODefault
        1 -> MFTVEOForce
        _ -> (error . showText) $ "Unknown index for M2tsForceTsVideoEbpOrder: " <> toText i
    fromEnum x = case x of
        MFTVEODefault -> 0
        MFTVEOForce -> 1
        M2tsForceTsVideoEbpOrder' name -> (error . showText) $ "Unknown M2tsForceTsVideoEbpOrder: " <> original name

-- | Represents the bounds of /known/ $M2tsForceTsVideoEbpOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsForceTsVideoEbpOrder where
    minBound = MFTVEODefault
    maxBound = MFTVEOForce

instance Hashable     M2tsForceTsVideoEbpOrder
instance NFData       M2tsForceTsVideoEbpOrder
instance ToByteString M2tsForceTsVideoEbpOrder
instance ToQuery      M2tsForceTsVideoEbpOrder
instance ToHeader     M2tsForceTsVideoEbpOrder

instance ToJSON M2tsForceTsVideoEbpOrder where
    toJSON = toJSONText

instance FromJSON M2tsForceTsVideoEbpOrder where
    parseJSON = parseJSONText "M2tsForceTsVideoEbpOrder"
