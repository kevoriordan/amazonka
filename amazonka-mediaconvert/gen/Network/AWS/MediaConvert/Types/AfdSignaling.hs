{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AfdSignaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AfdSignaling (
  AfdSignaling (
    ..
    , ASAuto
    , ASFixed
    , ASNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to specify whether the service includes AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
data AfdSignaling = AfdSignaling' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ASAuto :: AfdSignaling
pattern ASAuto = AfdSignaling' "AUTO"

pattern ASFixed :: AfdSignaling
pattern ASFixed = AfdSignaling' "FIXED"

pattern ASNone :: AfdSignaling
pattern ASNone = AfdSignaling' "NONE"

{-# COMPLETE
  ASAuto,
  ASFixed,
  ASNone,
  AfdSignaling' #-}

instance FromText AfdSignaling where
    parser = (AfdSignaling' . mk) <$> takeText

instance ToText AfdSignaling where
    toText (AfdSignaling' ci) = original ci

-- | Represents an enum of /known/ $AfdSignaling.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AfdSignaling where
    toEnum i = case i of
        0 -> ASAuto
        1 -> ASFixed
        2 -> ASNone
        _ -> (error . showText) $ "Unknown index for AfdSignaling: " <> toText i
    fromEnum x = case x of
        ASAuto -> 0
        ASFixed -> 1
        ASNone -> 2
        AfdSignaling' name -> (error . showText) $ "Unknown AfdSignaling: " <> original name

-- | Represents the bounds of /known/ $AfdSignaling.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AfdSignaling where
    minBound = ASAuto
    maxBound = ASNone

instance Hashable     AfdSignaling
instance NFData       AfdSignaling
instance ToByteString AfdSignaling
instance ToQuery      AfdSignaling
instance ToHeader     AfdSignaling

instance ToJSON AfdSignaling where
    toJSON = toJSONText

instance FromJSON AfdSignaling where
    parseJSON = parseJSONText "AfdSignaling"
