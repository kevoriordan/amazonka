{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SlowPal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2SlowPal (
  Mpeg2SlowPal (
    ..
    , MSPDisabled
    , MSPEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
data Mpeg2SlowPal = Mpeg2SlowPal' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern MSPDisabled :: Mpeg2SlowPal
pattern MSPDisabled = Mpeg2SlowPal' "DISABLED"

pattern MSPEnabled :: Mpeg2SlowPal
pattern MSPEnabled = Mpeg2SlowPal' "ENABLED"

{-# COMPLETE
  MSPDisabled,
  MSPEnabled,
  Mpeg2SlowPal' #-}

instance FromText Mpeg2SlowPal where
    parser = (Mpeg2SlowPal' . mk) <$> takeText

instance ToText Mpeg2SlowPal where
    toText (Mpeg2SlowPal' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2SlowPal.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2SlowPal where
    toEnum i = case i of
        0 -> MSPDisabled
        1 -> MSPEnabled
        _ -> (error . showText) $ "Unknown index for Mpeg2SlowPal: " <> toText i
    fromEnum x = case x of
        MSPDisabled -> 0
        MSPEnabled -> 1
        Mpeg2SlowPal' name -> (error . showText) $ "Unknown Mpeg2SlowPal: " <> original name

-- | Represents the bounds of /known/ $Mpeg2SlowPal.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2SlowPal where
    minBound = MSPDisabled
    maxBound = MSPEnabled

instance Hashable     Mpeg2SlowPal
instance NFData       Mpeg2SlowPal
instance ToByteString Mpeg2SlowPal
instance ToQuery      Mpeg2SlowPal
instance ToHeader     Mpeg2SlowPal

instance ToJSON Mpeg2SlowPal where
    toJSON = toJSONText

instance FromJSON Mpeg2SlowPal where
    parseJSON = parseJSONText "Mpeg2SlowPal"
