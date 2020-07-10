{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.LastFrameClippingBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.LastFrameClippingBehavior (
  LastFrameClippingBehavior (
    ..
    , ExcludeLastFrame
    , IncludeLastFrame
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
data LastFrameClippingBehavior = LastFrameClippingBehavior' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern ExcludeLastFrame :: LastFrameClippingBehavior
pattern ExcludeLastFrame = LastFrameClippingBehavior' "EXCLUDE_LAST_FRAME"

pattern IncludeLastFrame :: LastFrameClippingBehavior
pattern IncludeLastFrame = LastFrameClippingBehavior' "INCLUDE_LAST_FRAME"

{-# COMPLETE
  ExcludeLastFrame,
  IncludeLastFrame,
  LastFrameClippingBehavior' #-}

instance FromText LastFrameClippingBehavior where
    parser = (LastFrameClippingBehavior' . mk) <$> takeText

instance ToText LastFrameClippingBehavior where
    toText (LastFrameClippingBehavior' ci) = original ci

-- | Represents an enum of /known/ $LastFrameClippingBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LastFrameClippingBehavior where
    toEnum i = case i of
        0 -> ExcludeLastFrame
        1 -> IncludeLastFrame
        _ -> (error . showText) $ "Unknown index for LastFrameClippingBehavior: " <> toText i
    fromEnum x = case x of
        ExcludeLastFrame -> 0
        IncludeLastFrame -> 1
        LastFrameClippingBehavior' name -> (error . showText) $ "Unknown LastFrameClippingBehavior: " <> original name

-- | Represents the bounds of /known/ $LastFrameClippingBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LastFrameClippingBehavior where
    minBound = ExcludeLastFrame
    maxBound = IncludeLastFrame

instance Hashable     LastFrameClippingBehavior
instance NFData       LastFrameClippingBehavior
instance ToByteString LastFrameClippingBehavior
instance ToQuery      LastFrameClippingBehavior
instance ToHeader     LastFrameClippingBehavior

instance ToJSON LastFrameClippingBehavior where
    toJSON = toJSONText

instance FromJSON LastFrameClippingBehavior where
    parseJSON = parseJSONText "LastFrameClippingBehavior"
