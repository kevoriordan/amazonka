{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresParControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ProresParControl (
  ProresParControl (
    ..
    , PPCInitializeFromSource
    , PPCSpecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use (ProresParControl) to specify how the service determines the pixel aspect ratio. Set to Follow source (INITIALIZE_FROM_SOURCE) to use the pixel aspect ratio from the input.  To specify a different pixel aspect ratio: Using the console, choose it from the dropdown menu. Using the API, set ProresParControl to (SPECIFIED) and provide  for (ParNumerator) and (ParDenominator).
data ProresParControl = ProresParControl' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern PPCInitializeFromSource :: ProresParControl
pattern PPCInitializeFromSource = ProresParControl' "INITIALIZE_FROM_SOURCE"

pattern PPCSpecified :: ProresParControl
pattern PPCSpecified = ProresParControl' "SPECIFIED"

{-# COMPLETE
  PPCInitializeFromSource,
  PPCSpecified,
  ProresParControl' #-}

instance FromText ProresParControl where
    parser = (ProresParControl' . mk) <$> takeText

instance ToText ProresParControl where
    toText (ProresParControl' ci) = original ci

-- | Represents an enum of /known/ $ProresParControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProresParControl where
    toEnum i = case i of
        0 -> PPCInitializeFromSource
        1 -> PPCSpecified
        _ -> (error . showText) $ "Unknown index for ProresParControl: " <> toText i
    fromEnum x = case x of
        PPCInitializeFromSource -> 0
        PPCSpecified -> 1
        ProresParControl' name -> (error . showText) $ "Unknown ProresParControl: " <> original name

-- | Represents the bounds of /known/ $ProresParControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProresParControl where
    minBound = PPCInitializeFromSource
    maxBound = PPCSpecified

instance Hashable     ProresParControl
instance NFData       ProresParControl
instance ToByteString ProresParControl
instance ToQuery      ProresParControl
instance ToHeader     ProresParControl

instance ToJSON ProresParControl where
    toJSON = toJSONText

instance FromJSON ProresParControl where
    parseJSON = parseJSONText "ProresParControl"
