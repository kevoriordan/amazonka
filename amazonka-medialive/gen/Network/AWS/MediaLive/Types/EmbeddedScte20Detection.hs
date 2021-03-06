{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedScte20Detection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.EmbeddedScte20Detection (
  EmbeddedScte20Detection (
    ..
    , ESDAuto
    , ESDOff
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Embedded Scte20 Detection
data EmbeddedScte20Detection = EmbeddedScte20Detection' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern ESDAuto :: EmbeddedScte20Detection
pattern ESDAuto = EmbeddedScte20Detection' "AUTO"

pattern ESDOff :: EmbeddedScte20Detection
pattern ESDOff = EmbeddedScte20Detection' "OFF"

{-# COMPLETE
  ESDAuto,
  ESDOff,
  EmbeddedScte20Detection' #-}

instance FromText EmbeddedScte20Detection where
    parser = (EmbeddedScte20Detection' . mk) <$> takeText

instance ToText EmbeddedScte20Detection where
    toText (EmbeddedScte20Detection' ci) = original ci

-- | Represents an enum of /known/ $EmbeddedScte20Detection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EmbeddedScte20Detection where
    toEnum i = case i of
        0 -> ESDAuto
        1 -> ESDOff
        _ -> (error . showText) $ "Unknown index for EmbeddedScte20Detection: " <> toText i
    fromEnum x = case x of
        ESDAuto -> 0
        ESDOff -> 1
        EmbeddedScte20Detection' name -> (error . showText) $ "Unknown EmbeddedScte20Detection: " <> original name

-- | Represents the bounds of /known/ $EmbeddedScte20Detection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EmbeddedScte20Detection where
    minBound = ESDAuto
    maxBound = ESDOff

instance Hashable     EmbeddedScte20Detection
instance NFData       EmbeddedScte20Detection
instance ToByteString EmbeddedScte20Detection
instance ToQuery      EmbeddedScte20Detection
instance ToHeader     EmbeddedScte20Detection

instance ToJSON EmbeddedScte20Detection where
    toJSON = toJSONText

instance FromJSON EmbeddedScte20Detection where
    parseJSON = parseJSONText "EmbeddedScte20Detection"
