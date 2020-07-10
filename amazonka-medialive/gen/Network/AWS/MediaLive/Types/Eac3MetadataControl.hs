{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3MetadataControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3MetadataControl (
  Eac3MetadataControl (
    ..
    , EMCFollowInput
    , EMCUseConfigured
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Eac3 Metadata Control
data Eac3MetadataControl = Eac3MetadataControl' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern EMCFollowInput :: Eac3MetadataControl
pattern EMCFollowInput = Eac3MetadataControl' "FOLLOW_INPUT"

pattern EMCUseConfigured :: Eac3MetadataControl
pattern EMCUseConfigured = Eac3MetadataControl' "USE_CONFIGURED"

{-# COMPLETE
  EMCFollowInput,
  EMCUseConfigured,
  Eac3MetadataControl' #-}

instance FromText Eac3MetadataControl where
    parser = (Eac3MetadataControl' . mk) <$> takeText

instance ToText Eac3MetadataControl where
    toText (Eac3MetadataControl' ci) = original ci

-- | Represents an enum of /known/ $Eac3MetadataControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3MetadataControl where
    toEnum i = case i of
        0 -> EMCFollowInput
        1 -> EMCUseConfigured
        _ -> (error . showText) $ "Unknown index for Eac3MetadataControl: " <> toText i
    fromEnum x = case x of
        EMCFollowInput -> 0
        EMCUseConfigured -> 1
        Eac3MetadataControl' name -> (error . showText) $ "Unknown Eac3MetadataControl: " <> original name

-- | Represents the bounds of /known/ $Eac3MetadataControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3MetadataControl where
    minBound = EMCFollowInput
    maxBound = EMCUseConfigured

instance Hashable     Eac3MetadataControl
instance NFData       Eac3MetadataControl
instance ToByteString Eac3MetadataControl
instance ToQuery      Eac3MetadataControl
instance ToHeader     Eac3MetadataControl

instance ToJSON Eac3MetadataControl where
    toJSON = toJSONText

instance FromJSON Eac3MetadataControl where
    parseJSON = parseJSONText "Eac3MetadataControl"
