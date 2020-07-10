{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265ScanType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265ScanType (
  H265ScanType (
    ..
    , Progressive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H265 Scan Type
data H265ScanType = H265ScanType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Progressive :: H265ScanType
pattern Progressive = H265ScanType' "PROGRESSIVE"

{-# COMPLETE
  Progressive,
  H265ScanType' #-}

instance FromText H265ScanType where
    parser = (H265ScanType' . mk) <$> takeText

instance ToText H265ScanType where
    toText (H265ScanType' ci) = original ci

-- | Represents an enum of /known/ $H265ScanType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265ScanType where
    toEnum i = case i of
        0 -> Progressive
        _ -> (error . showText) $ "Unknown index for H265ScanType: " <> toText i
    fromEnum x = case x of
        Progressive -> 0
        H265ScanType' name -> (error . showText) $ "Unknown H265ScanType: " <> original name

-- | Represents the bounds of /known/ $H265ScanType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265ScanType where
    minBound = Progressive
    maxBound = Progressive

instance Hashable     H265ScanType
instance NFData       H265ScanType
instance ToByteString H265ScanType
instance ToQuery      H265ScanType
instance ToHeader     H265ScanType

instance ToJSON H265ScanType where
    toJSON = toJSONText

instance FromJSON H265ScanType where
    parseJSON = parseJSONText "H265ScanType"
