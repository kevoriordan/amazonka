{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior (
  M2tsTimedMetadataBehavior (
    ..
    , MTMBNoPassthrough
    , MTMBPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M2ts Timed Metadata Behavior
data M2tsTimedMetadataBehavior = M2tsTimedMetadataBehavior' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern MTMBNoPassthrough :: M2tsTimedMetadataBehavior
pattern MTMBNoPassthrough = M2tsTimedMetadataBehavior' "NO_PASSTHROUGH"

pattern MTMBPassthrough :: M2tsTimedMetadataBehavior
pattern MTMBPassthrough = M2tsTimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  MTMBNoPassthrough,
  MTMBPassthrough,
  M2tsTimedMetadataBehavior' #-}

instance FromText M2tsTimedMetadataBehavior where
    parser = (M2tsTimedMetadataBehavior' . mk) <$> takeText

instance ToText M2tsTimedMetadataBehavior where
    toText (M2tsTimedMetadataBehavior' ci) = original ci

-- | Represents an enum of /known/ $M2tsTimedMetadataBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsTimedMetadataBehavior where
    toEnum i = case i of
        0 -> MTMBNoPassthrough
        1 -> MTMBPassthrough
        _ -> (error . showText) $ "Unknown index for M2tsTimedMetadataBehavior: " <> toText i
    fromEnum x = case x of
        MTMBNoPassthrough -> 0
        MTMBPassthrough -> 1
        M2tsTimedMetadataBehavior' name -> (error . showText) $ "Unknown M2tsTimedMetadataBehavior: " <> original name

-- | Represents the bounds of /known/ $M2tsTimedMetadataBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsTimedMetadataBehavior where
    minBound = MTMBNoPassthrough
    maxBound = MTMBPassthrough

instance Hashable     M2tsTimedMetadataBehavior
instance NFData       M2tsTimedMetadataBehavior
instance ToByteString M2tsTimedMetadataBehavior
instance ToQuery      M2tsTimedMetadataBehavior
instance ToHeader     M2tsTimedMetadataBehavior

instance ToJSON M2tsTimedMetadataBehavior where
    toJSON = toJSONText

instance FromJSON M2tsTimedMetadataBehavior where
    parseJSON = parseJSONText "M2tsTimedMetadataBehavior"
