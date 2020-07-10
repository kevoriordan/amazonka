{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Profile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265Profile (
  H265Profile (
    ..
    , Main
    , Main10BIT
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H265 Profile
data H265Profile = H265Profile' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Main :: H265Profile
pattern Main = H265Profile' "MAIN"

pattern Main10BIT :: H265Profile
pattern Main10BIT = H265Profile' "MAIN_10BIT"

{-# COMPLETE
  Main,
  Main10BIT,
  H265Profile' #-}

instance FromText H265Profile where
    parser = (H265Profile' . mk) <$> takeText

instance ToText H265Profile where
    toText (H265Profile' ci) = original ci

-- | Represents an enum of /known/ $H265Profile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265Profile where
    toEnum i = case i of
        0 -> Main
        1 -> Main10BIT
        _ -> (error . showText) $ "Unknown index for H265Profile: " <> toText i
    fromEnum x = case x of
        Main -> 0
        Main10BIT -> 1
        H265Profile' name -> (error . showText) $ "Unknown H265Profile: " <> original name

-- | Represents the bounds of /known/ $H265Profile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265Profile where
    minBound = Main
    maxBound = Main10BIT

instance Hashable     H265Profile
instance NFData       H265Profile
instance ToByteString H265Profile
instance ToQuery      H265Profile
instance ToHeader     H265Profile

instance ToJSON H265Profile where
    toJSON = toJSONText

instance FromJSON H265Profile where
    parseJSON = parseJSONText "H265Profile"
