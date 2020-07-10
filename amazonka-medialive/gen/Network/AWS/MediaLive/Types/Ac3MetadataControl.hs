{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3MetadataControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Ac3MetadataControl (
  Ac3MetadataControl (
    ..
    , AMCFollowInput
    , AMCUseConfigured
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Ac3 Metadata Control
data Ac3MetadataControl = Ac3MetadataControl' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern AMCFollowInput :: Ac3MetadataControl
pattern AMCFollowInput = Ac3MetadataControl' "FOLLOW_INPUT"

pattern AMCUseConfigured :: Ac3MetadataControl
pattern AMCUseConfigured = Ac3MetadataControl' "USE_CONFIGURED"

{-# COMPLETE
  AMCFollowInput,
  AMCUseConfigured,
  Ac3MetadataControl' #-}

instance FromText Ac3MetadataControl where
    parser = (Ac3MetadataControl' . mk) <$> takeText

instance ToText Ac3MetadataControl where
    toText (Ac3MetadataControl' ci) = original ci

-- | Represents an enum of /known/ $Ac3MetadataControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Ac3MetadataControl where
    toEnum i = case i of
        0 -> AMCFollowInput
        1 -> AMCUseConfigured
        _ -> (error . showText) $ "Unknown index for Ac3MetadataControl: " <> toText i
    fromEnum x = case x of
        AMCFollowInput -> 0
        AMCUseConfigured -> 1
        Ac3MetadataControl' name -> (error . showText) $ "Unknown Ac3MetadataControl: " <> original name

-- | Represents the bounds of /known/ $Ac3MetadataControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Ac3MetadataControl where
    minBound = AMCFollowInput
    maxBound = AMCUseConfigured

instance Hashable     Ac3MetadataControl
instance NFData       Ac3MetadataControl
instance ToByteString Ac3MetadataControl
instance ToQuery      Ac3MetadataControl
instance ToHeader     Ac3MetadataControl

instance ToJSON Ac3MetadataControl where
    toJSON = toJSONText

instance FromJSON Ac3MetadataControl where
    parseJSON = parseJSONText "Ac3MetadataControl"
