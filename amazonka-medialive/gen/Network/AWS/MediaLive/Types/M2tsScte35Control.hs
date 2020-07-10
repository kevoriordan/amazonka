{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsScte35Control
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsScte35Control (
  M2tsScte35Control (
    ..
    , MSCNone
    , MSCPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M2ts Scte35 Control
data M2tsScte35Control = M2tsScte35Control' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern MSCNone :: M2tsScte35Control
pattern MSCNone = M2tsScte35Control' "NONE"

pattern MSCPassthrough :: M2tsScte35Control
pattern MSCPassthrough = M2tsScte35Control' "PASSTHROUGH"

{-# COMPLETE
  MSCNone,
  MSCPassthrough,
  M2tsScte35Control' #-}

instance FromText M2tsScte35Control where
    parser = (M2tsScte35Control' . mk) <$> takeText

instance ToText M2tsScte35Control where
    toText (M2tsScte35Control' ci) = original ci

-- | Represents an enum of /known/ $M2tsScte35Control.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsScte35Control where
    toEnum i = case i of
        0 -> MSCNone
        1 -> MSCPassthrough
        _ -> (error . showText) $ "Unknown index for M2tsScte35Control: " <> toText i
    fromEnum x = case x of
        MSCNone -> 0
        MSCPassthrough -> 1
        M2tsScte35Control' name -> (error . showText) $ "Unknown M2tsScte35Control: " <> original name

-- | Represents the bounds of /known/ $M2tsScte35Control.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsScte35Control where
    minBound = MSCNone
    maxBound = MSCPassthrough

instance Hashable     M2tsScte35Control
instance NFData       M2tsScte35Control
instance ToByteString M2tsScte35Control
instance ToQuery      M2tsScte35Control
instance ToHeader     M2tsScte35Control

instance ToJSON M2tsScte35Control where
    toJSON = toJSONText

instance FromJSON M2tsScte35Control where
    parseJSON = parseJSONText "M2tsScte35Control"
