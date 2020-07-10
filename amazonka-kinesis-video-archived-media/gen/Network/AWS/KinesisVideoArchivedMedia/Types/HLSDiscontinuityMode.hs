{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode (
  HLSDiscontinuityMode (
    ..
    , Always
    , Never
    , OnDiscontinuity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HLSDiscontinuityMode = HLSDiscontinuityMode' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Always :: HLSDiscontinuityMode
pattern Always = HLSDiscontinuityMode' "ALWAYS"

pattern Never :: HLSDiscontinuityMode
pattern Never = HLSDiscontinuityMode' "NEVER"

pattern OnDiscontinuity :: HLSDiscontinuityMode
pattern OnDiscontinuity = HLSDiscontinuityMode' "ON_DISCONTINUITY"

{-# COMPLETE
  Always,
  Never,
  OnDiscontinuity,
  HLSDiscontinuityMode' #-}

instance FromText HLSDiscontinuityMode where
    parser = (HLSDiscontinuityMode' . mk) <$> takeText

instance ToText HLSDiscontinuityMode where
    toText (HLSDiscontinuityMode' ci) = original ci

-- | Represents an enum of /known/ $HLSDiscontinuityMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HLSDiscontinuityMode where
    toEnum i = case i of
        0 -> Always
        1 -> Never
        2 -> OnDiscontinuity
        _ -> (error . showText) $ "Unknown index for HLSDiscontinuityMode: " <> toText i
    fromEnum x = case x of
        Always -> 0
        Never -> 1
        OnDiscontinuity -> 2
        HLSDiscontinuityMode' name -> (error . showText) $ "Unknown HLSDiscontinuityMode: " <> original name

-- | Represents the bounds of /known/ $HLSDiscontinuityMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HLSDiscontinuityMode where
    minBound = Always
    maxBound = OnDiscontinuity

instance Hashable     HLSDiscontinuityMode
instance NFData       HLSDiscontinuityMode
instance ToByteString HLSDiscontinuityMode
instance ToQuery      HLSDiscontinuityMode
instance ToHeader     HLSDiscontinuityMode

instance ToJSON HLSDiscontinuityMode where
    toJSON = toJSONText
