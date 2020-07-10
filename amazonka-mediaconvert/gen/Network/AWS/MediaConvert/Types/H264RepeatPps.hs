{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264RepeatPps
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264RepeatPps (
  H264RepeatPps (
    ..
    , HRPDisabled
    , HRPEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Places a PPS header on each encoded picture, even if repeated.
data H264RepeatPps = H264RepeatPps' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern HRPDisabled :: H264RepeatPps
pattern HRPDisabled = H264RepeatPps' "DISABLED"

pattern HRPEnabled :: H264RepeatPps
pattern HRPEnabled = H264RepeatPps' "ENABLED"

{-# COMPLETE
  HRPDisabled,
  HRPEnabled,
  H264RepeatPps' #-}

instance FromText H264RepeatPps where
    parser = (H264RepeatPps' . mk) <$> takeText

instance ToText H264RepeatPps where
    toText (H264RepeatPps' ci) = original ci

-- | Represents an enum of /known/ $H264RepeatPps.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264RepeatPps where
    toEnum i = case i of
        0 -> HRPDisabled
        1 -> HRPEnabled
        _ -> (error . showText) $ "Unknown index for H264RepeatPps: " <> toText i
    fromEnum x = case x of
        HRPDisabled -> 0
        HRPEnabled -> 1
        H264RepeatPps' name -> (error . showText) $ "Unknown H264RepeatPps: " <> original name

-- | Represents the bounds of /known/ $H264RepeatPps.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264RepeatPps where
    minBound = HRPDisabled
    maxBound = HRPEnabled

instance Hashable     H264RepeatPps
instance NFData       H264RepeatPps
instance ToByteString H264RepeatPps
instance ToQuery      H264RepeatPps
instance ToHeader     H264RepeatPps

instance ToJSON H264RepeatPps where
    toJSON = toJSONText

instance FromJSON H264RepeatPps where
    parseJSON = parseJSONText "H264RepeatPps"
