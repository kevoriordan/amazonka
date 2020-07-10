{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior (
  M2tsNielsenId3Behavior (
    ..
    , MNIBNoPassthrough
    , MNIBPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M2ts Nielsen Id3 Behavior
data M2tsNielsenId3Behavior = M2tsNielsenId3Behavior' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern MNIBNoPassthrough :: M2tsNielsenId3Behavior
pattern MNIBNoPassthrough = M2tsNielsenId3Behavior' "NO_PASSTHROUGH"

pattern MNIBPassthrough :: M2tsNielsenId3Behavior
pattern MNIBPassthrough = M2tsNielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE
  MNIBNoPassthrough,
  MNIBPassthrough,
  M2tsNielsenId3Behavior' #-}

instance FromText M2tsNielsenId3Behavior where
    parser = (M2tsNielsenId3Behavior' . mk) <$> takeText

instance ToText M2tsNielsenId3Behavior where
    toText (M2tsNielsenId3Behavior' ci) = original ci

-- | Represents an enum of /known/ $M2tsNielsenId3Behavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsNielsenId3Behavior where
    toEnum i = case i of
        0 -> MNIBNoPassthrough
        1 -> MNIBPassthrough
        _ -> (error . showText) $ "Unknown index for M2tsNielsenId3Behavior: " <> toText i
    fromEnum x = case x of
        MNIBNoPassthrough -> 0
        MNIBPassthrough -> 1
        M2tsNielsenId3Behavior' name -> (error . showText) $ "Unknown M2tsNielsenId3Behavior: " <> original name

-- | Represents the bounds of /known/ $M2tsNielsenId3Behavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsNielsenId3Behavior where
    minBound = MNIBNoPassthrough
    maxBound = MNIBPassthrough

instance Hashable     M2tsNielsenId3Behavior
instance NFData       M2tsNielsenId3Behavior
instance ToByteString M2tsNielsenId3Behavior
instance ToQuery      M2tsNielsenId3Behavior
instance ToHeader     M2tsNielsenId3Behavior

instance ToJSON M2tsNielsenId3Behavior where
    toJSON = toJSONText

instance FromJSON M2tsNielsenId3Behavior where
    parseJSON = parseJSONText "M2tsNielsenId3Behavior"
