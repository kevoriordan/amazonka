{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAudioStreamType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsAudioStreamType (
  M2tsAudioStreamType (
    ..
    , MASTAtsc
    , MASTDvb
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M2ts Audio Stream Type
data M2tsAudioStreamType = M2tsAudioStreamType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern MASTAtsc :: M2tsAudioStreamType
pattern MASTAtsc = M2tsAudioStreamType' "ATSC"

pattern MASTDvb :: M2tsAudioStreamType
pattern MASTDvb = M2tsAudioStreamType' "DVB"

{-# COMPLETE
  MASTAtsc,
  MASTDvb,
  M2tsAudioStreamType' #-}

instance FromText M2tsAudioStreamType where
    parser = (M2tsAudioStreamType' . mk) <$> takeText

instance ToText M2tsAudioStreamType where
    toText (M2tsAudioStreamType' ci) = original ci

-- | Represents an enum of /known/ $M2tsAudioStreamType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsAudioStreamType where
    toEnum i = case i of
        0 -> MASTAtsc
        1 -> MASTDvb
        _ -> (error . showText) $ "Unknown index for M2tsAudioStreamType: " <> toText i
    fromEnum x = case x of
        MASTAtsc -> 0
        MASTDvb -> 1
        M2tsAudioStreamType' name -> (error . showText) $ "Unknown M2tsAudioStreamType: " <> original name

-- | Represents the bounds of /known/ $M2tsAudioStreamType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsAudioStreamType where
    minBound = MASTAtsc
    maxBound = MASTDvb

instance Hashable     M2tsAudioStreamType
instance NFData       M2tsAudioStreamType
instance ToByteString M2tsAudioStreamType
instance ToQuery      M2tsAudioStreamType
instance ToHeader     M2tsAudioStreamType

instance ToJSON M2tsAudioStreamType where
    toJSON = toJSONText

instance FromJSON M2tsAudioStreamType where
    parseJSON = parseJSONText "M2tsAudioStreamType"
