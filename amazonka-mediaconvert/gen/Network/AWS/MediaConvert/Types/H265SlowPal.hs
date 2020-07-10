{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265SlowPal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265SlowPal (
  H265SlowPal (
    ..
    , HSPDisabled
    , HSPEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
data H265SlowPal = H265SlowPal' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern HSPDisabled :: H265SlowPal
pattern HSPDisabled = H265SlowPal' "DISABLED"

pattern HSPEnabled :: H265SlowPal
pattern HSPEnabled = H265SlowPal' "ENABLED"

{-# COMPLETE
  HSPDisabled,
  HSPEnabled,
  H265SlowPal' #-}

instance FromText H265SlowPal where
    parser = (H265SlowPal' . mk) <$> takeText

instance ToText H265SlowPal where
    toText (H265SlowPal' ci) = original ci

-- | Represents an enum of /known/ $H265SlowPal.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265SlowPal where
    toEnum i = case i of
        0 -> HSPDisabled
        1 -> HSPEnabled
        _ -> (error . showText) $ "Unknown index for H265SlowPal: " <> toText i
    fromEnum x = case x of
        HSPDisabled -> 0
        HSPEnabled -> 1
        H265SlowPal' name -> (error . showText) $ "Unknown H265SlowPal: " <> original name

-- | Represents the bounds of /known/ $H265SlowPal.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265SlowPal where
    minBound = HSPDisabled
    maxBound = HSPEnabled

instance Hashable     H265SlowPal
instance NFData       H265SlowPal
instance ToByteString H265SlowPal
instance ToQuery      H265SlowPal
instance ToHeader     H265SlowPal

instance ToJSON H265SlowPal where
    toJSON = toJSONText

instance FromJSON H265SlowPal where
    parseJSON = parseJSONText "H265SlowPal"
