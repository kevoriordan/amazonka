{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions (
  AncillaryTerminateCaptions (
    ..
    , ATCDisabled
    , ATCEndOfInput
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
data AncillaryTerminateCaptions = AncillaryTerminateCaptions' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern ATCDisabled :: AncillaryTerminateCaptions
pattern ATCDisabled = AncillaryTerminateCaptions' "DISABLED"

pattern ATCEndOfInput :: AncillaryTerminateCaptions
pattern ATCEndOfInput = AncillaryTerminateCaptions' "END_OF_INPUT"

{-# COMPLETE
  ATCDisabled,
  ATCEndOfInput,
  AncillaryTerminateCaptions' #-}

instance FromText AncillaryTerminateCaptions where
    parser = (AncillaryTerminateCaptions' . mk) <$> takeText

instance ToText AncillaryTerminateCaptions where
    toText (AncillaryTerminateCaptions' ci) = original ci

-- | Represents an enum of /known/ $AncillaryTerminateCaptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AncillaryTerminateCaptions where
    toEnum i = case i of
        0 -> ATCDisabled
        1 -> ATCEndOfInput
        _ -> (error . showText) $ "Unknown index for AncillaryTerminateCaptions: " <> toText i
    fromEnum x = case x of
        ATCDisabled -> 0
        ATCEndOfInput -> 1
        AncillaryTerminateCaptions' name -> (error . showText) $ "Unknown AncillaryTerminateCaptions: " <> original name

-- | Represents the bounds of /known/ $AncillaryTerminateCaptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AncillaryTerminateCaptions where
    minBound = ATCDisabled
    maxBound = ATCEndOfInput

instance Hashable     AncillaryTerminateCaptions
instance NFData       AncillaryTerminateCaptions
instance ToByteString AncillaryTerminateCaptions
instance ToQuery      AncillaryTerminateCaptions
instance ToHeader     AncillaryTerminateCaptions

instance ToJSON AncillaryTerminateCaptions where
    toJSON = toJSONText

instance FromJSON AncillaryTerminateCaptions where
    parseJSON = parseJSONText "AncillaryTerminateCaptions"
