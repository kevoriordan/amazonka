{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior (
  M3u8TimedMetadataBehavior (
    ..
    , M3uNoPassthrough
    , M3uPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M3u8 Timed Metadata Behavior
data M3u8TimedMetadataBehavior = M3u8TimedMetadataBehavior' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern M3uNoPassthrough :: M3u8TimedMetadataBehavior
pattern M3uNoPassthrough = M3u8TimedMetadataBehavior' "NO_PASSTHROUGH"

pattern M3uPassthrough :: M3u8TimedMetadataBehavior
pattern M3uPassthrough = M3u8TimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  M3uNoPassthrough,
  M3uPassthrough,
  M3u8TimedMetadataBehavior' #-}

instance FromText M3u8TimedMetadataBehavior where
    parser = (M3u8TimedMetadataBehavior' . mk) <$> takeText

instance ToText M3u8TimedMetadataBehavior where
    toText (M3u8TimedMetadataBehavior' ci) = original ci

-- | Represents an enum of /known/ $M3u8TimedMetadataBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M3u8TimedMetadataBehavior where
    toEnum i = case i of
        0 -> M3uNoPassthrough
        1 -> M3uPassthrough
        _ -> (error . showText) $ "Unknown index for M3u8TimedMetadataBehavior: " <> toText i
    fromEnum x = case x of
        M3uNoPassthrough -> 0
        M3uPassthrough -> 1
        M3u8TimedMetadataBehavior' name -> (error . showText) $ "Unknown M3u8TimedMetadataBehavior: " <> original name

-- | Represents the bounds of /known/ $M3u8TimedMetadataBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M3u8TimedMetadataBehavior where
    minBound = M3uNoPassthrough
    maxBound = M3uPassthrough

instance Hashable     M3u8TimedMetadataBehavior
instance NFData       M3u8TimedMetadataBehavior
instance ToByteString M3u8TimedMetadataBehavior
instance ToQuery      M3u8TimedMetadataBehavior
instance ToHeader     M3u8TimedMetadataBehavior

instance ToJSON M3u8TimedMetadataBehavior where
    toJSON = toJSONText

instance FromJSON M3u8TimedMetadataBehavior where
    parseJSON = parseJSONText "M3u8TimedMetadataBehavior"
