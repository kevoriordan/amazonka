{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ScalingBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ScalingBehavior (
  ScalingBehavior (
    ..
    , SBDefault
    , SBStretchToOutput
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify how the service handles outputs that have a different aspect ratio from the input aspect ratio. Choose Stretch to output (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit. Keep the setting Default (DEFAULT) to have the service letterbox your video instead. This setting overrides any value that you specify for the setting Selection placement (position) in this output.
data ScalingBehavior = ScalingBehavior' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern SBDefault :: ScalingBehavior
pattern SBDefault = ScalingBehavior' "DEFAULT"

pattern SBStretchToOutput :: ScalingBehavior
pattern SBStretchToOutput = ScalingBehavior' "STRETCH_TO_OUTPUT"

{-# COMPLETE
  SBDefault,
  SBStretchToOutput,
  ScalingBehavior' #-}

instance FromText ScalingBehavior where
    parser = (ScalingBehavior' . mk) <$> takeText

instance ToText ScalingBehavior where
    toText (ScalingBehavior' ci) = original ci

-- | Represents an enum of /known/ $ScalingBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScalingBehavior where
    toEnum i = case i of
        0 -> SBDefault
        1 -> SBStretchToOutput
        _ -> (error . showText) $ "Unknown index for ScalingBehavior: " <> toText i
    fromEnum x = case x of
        SBDefault -> 0
        SBStretchToOutput -> 1
        ScalingBehavior' name -> (error . showText) $ "Unknown ScalingBehavior: " <> original name

-- | Represents the bounds of /known/ $ScalingBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScalingBehavior where
    minBound = SBDefault
    maxBound = SBStretchToOutput

instance Hashable     ScalingBehavior
instance NFData       ScalingBehavior
instance ToByteString ScalingBehavior
instance ToQuery      ScalingBehavior
instance ToHeader     ScalingBehavior

instance ToJSON ScalingBehavior where
    toJSON = toJSONText

instance FromJSON ScalingBehavior where
    parseJSON = parseJSONText "ScalingBehavior"
