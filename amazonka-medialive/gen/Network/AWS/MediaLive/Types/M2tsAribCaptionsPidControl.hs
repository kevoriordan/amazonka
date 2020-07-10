{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl (
  M2tsAribCaptionsPidControl (
    ..
    , MACPCAuto
    , MACPCUseConfigured
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M2ts Arib Captions Pid Control
data M2tsAribCaptionsPidControl = M2tsAribCaptionsPidControl' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern MACPCAuto :: M2tsAribCaptionsPidControl
pattern MACPCAuto = M2tsAribCaptionsPidControl' "AUTO"

pattern MACPCUseConfigured :: M2tsAribCaptionsPidControl
pattern MACPCUseConfigured = M2tsAribCaptionsPidControl' "USE_CONFIGURED"

{-# COMPLETE
  MACPCAuto,
  MACPCUseConfigured,
  M2tsAribCaptionsPidControl' #-}

instance FromText M2tsAribCaptionsPidControl where
    parser = (M2tsAribCaptionsPidControl' . mk) <$> takeText

instance ToText M2tsAribCaptionsPidControl where
    toText (M2tsAribCaptionsPidControl' ci) = original ci

-- | Represents an enum of /known/ $M2tsAribCaptionsPidControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsAribCaptionsPidControl where
    toEnum i = case i of
        0 -> MACPCAuto
        1 -> MACPCUseConfigured
        _ -> (error . showText) $ "Unknown index for M2tsAribCaptionsPidControl: " <> toText i
    fromEnum x = case x of
        MACPCAuto -> 0
        MACPCUseConfigured -> 1
        M2tsAribCaptionsPidControl' name -> (error . showText) $ "Unknown M2tsAribCaptionsPidControl: " <> original name

-- | Represents the bounds of /known/ $M2tsAribCaptionsPidControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsAribCaptionsPidControl where
    minBound = MACPCAuto
    maxBound = MACPCUseConfigured

instance Hashable     M2tsAribCaptionsPidControl
instance NFData       M2tsAribCaptionsPidControl
instance ToByteString M2tsAribCaptionsPidControl
instance ToQuery      M2tsAribCaptionsPidControl
instance ToHeader     M2tsAribCaptionsPidControl

instance ToJSON M2tsAribCaptionsPidControl where
    toJSON = toJSONText

instance FromJSON M2tsAribCaptionsPidControl where
    parseJSON = parseJSONText "M2tsAribCaptionsPidControl"
