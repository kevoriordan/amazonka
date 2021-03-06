{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AncillaryConvert608To708
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AncillaryConvert608To708 (
  AncillaryConvert608To708 (
    ..
    , ACTDisabled
    , ACTUpconvert
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
data AncillaryConvert608To708 = AncillaryConvert608To708' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern ACTDisabled :: AncillaryConvert608To708
pattern ACTDisabled = AncillaryConvert608To708' "DISABLED"

pattern ACTUpconvert :: AncillaryConvert608To708
pattern ACTUpconvert = AncillaryConvert608To708' "UPCONVERT"

{-# COMPLETE
  ACTDisabled,
  ACTUpconvert,
  AncillaryConvert608To708' #-}

instance FromText AncillaryConvert608To708 where
    parser = (AncillaryConvert608To708' . mk) <$> takeText

instance ToText AncillaryConvert608To708 where
    toText (AncillaryConvert608To708' ci) = original ci

-- | Represents an enum of /known/ $AncillaryConvert608To708.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AncillaryConvert608To708 where
    toEnum i = case i of
        0 -> ACTDisabled
        1 -> ACTUpconvert
        _ -> (error . showText) $ "Unknown index for AncillaryConvert608To708: " <> toText i
    fromEnum x = case x of
        ACTDisabled -> 0
        ACTUpconvert -> 1
        AncillaryConvert608To708' name -> (error . showText) $ "Unknown AncillaryConvert608To708: " <> original name

-- | Represents the bounds of /known/ $AncillaryConvert608To708.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AncillaryConvert608To708 where
    minBound = ACTDisabled
    maxBound = ACTUpconvert

instance Hashable     AncillaryConvert608To708
instance NFData       AncillaryConvert608To708
instance ToByteString AncillaryConvert608To708
instance ToQuery      AncillaryConvert608To708
instance ToHeader     AncillaryConvert608To708

instance ToJSON AncillaryConvert608To708 where
    toJSON = toJSONText

instance FromJSON AncillaryConvert608To708 where
    parseJSON = parseJSONText "AncillaryConvert608To708"
