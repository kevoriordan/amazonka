{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions (
  EmbeddedTerminateCaptions (
    ..
    , ETCDisabled
    , ETCEndOfInput
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
data EmbeddedTerminateCaptions = EmbeddedTerminateCaptions' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern ETCDisabled :: EmbeddedTerminateCaptions
pattern ETCDisabled = EmbeddedTerminateCaptions' "DISABLED"

pattern ETCEndOfInput :: EmbeddedTerminateCaptions
pattern ETCEndOfInput = EmbeddedTerminateCaptions' "END_OF_INPUT"

{-# COMPLETE
  ETCDisabled,
  ETCEndOfInput,
  EmbeddedTerminateCaptions' #-}

instance FromText EmbeddedTerminateCaptions where
    parser = (EmbeddedTerminateCaptions' . mk) <$> takeText

instance ToText EmbeddedTerminateCaptions where
    toText (EmbeddedTerminateCaptions' ci) = original ci

-- | Represents an enum of /known/ $EmbeddedTerminateCaptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EmbeddedTerminateCaptions where
    toEnum i = case i of
        0 -> ETCDisabled
        1 -> ETCEndOfInput
        _ -> (error . showText) $ "Unknown index for EmbeddedTerminateCaptions: " <> toText i
    fromEnum x = case x of
        ETCDisabled -> 0
        ETCEndOfInput -> 1
        EmbeddedTerminateCaptions' name -> (error . showText) $ "Unknown EmbeddedTerminateCaptions: " <> original name

-- | Represents the bounds of /known/ $EmbeddedTerminateCaptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EmbeddedTerminateCaptions where
    minBound = ETCDisabled
    maxBound = ETCEndOfInput

instance Hashable     EmbeddedTerminateCaptions
instance NFData       EmbeddedTerminateCaptions
instance ToByteString EmbeddedTerminateCaptions
instance ToQuery      EmbeddedTerminateCaptions
instance ToHeader     EmbeddedTerminateCaptions

instance ToJSON EmbeddedTerminateCaptions where
    toJSON = toJSONText

instance FromJSON EmbeddedTerminateCaptions where
    parseJSON = parseJSONText "EmbeddedTerminateCaptions"
