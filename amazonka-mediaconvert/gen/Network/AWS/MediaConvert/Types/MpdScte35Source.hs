{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdScte35Source
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MpdScte35Source (
  MpdScte35Source (
    ..
    , MpdNone
    , MpdPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
data MpdScte35Source = MpdScte35Source' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern MpdNone :: MpdScte35Source
pattern MpdNone = MpdScte35Source' "NONE"

pattern MpdPassthrough :: MpdScte35Source
pattern MpdPassthrough = MpdScte35Source' "PASSTHROUGH"

{-# COMPLETE
  MpdNone,
  MpdPassthrough,
  MpdScte35Source' #-}

instance FromText MpdScte35Source where
    parser = (MpdScte35Source' . mk) <$> takeText

instance ToText MpdScte35Source where
    toText (MpdScte35Source' ci) = original ci

-- | Represents an enum of /known/ $MpdScte35Source.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MpdScte35Source where
    toEnum i = case i of
        0 -> MpdNone
        1 -> MpdPassthrough
        _ -> (error . showText) $ "Unknown index for MpdScte35Source: " <> toText i
    fromEnum x = case x of
        MpdNone -> 0
        MpdPassthrough -> 1
        MpdScte35Source' name -> (error . showText) $ "Unknown MpdScte35Source: " <> original name

-- | Represents the bounds of /known/ $MpdScte35Source.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MpdScte35Source where
    minBound = MpdNone
    maxBound = MpdPassthrough

instance Hashable     MpdScte35Source
instance NFData       MpdScte35Source
instance ToByteString MpdScte35Source
instance ToQuery      MpdScte35Source
instance ToHeader     MpdScte35Source

instance ToJSON MpdScte35Source where
    toJSON = toJSONText

instance FromJSON MpdScte35Source where
    parseJSON = parseJSONText "MpdScte35Source"
