{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264GopBReference
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264GopBReference (
  H264GopBReference (
    ..
    , HGBRDisabled
    , HGBREnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Gop BReference
data H264GopBReference = H264GopBReference' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern HGBRDisabled :: H264GopBReference
pattern HGBRDisabled = H264GopBReference' "DISABLED"

pattern HGBREnabled :: H264GopBReference
pattern HGBREnabled = H264GopBReference' "ENABLED"

{-# COMPLETE
  HGBRDisabled,
  HGBREnabled,
  H264GopBReference' #-}

instance FromText H264GopBReference where
    parser = (H264GopBReference' . mk) <$> takeText

instance ToText H264GopBReference where
    toText (H264GopBReference' ci) = original ci

-- | Represents an enum of /known/ $H264GopBReference.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264GopBReference where
    toEnum i = case i of
        0 -> HGBRDisabled
        1 -> HGBREnabled
        _ -> (error . showText) $ "Unknown index for H264GopBReference: " <> toText i
    fromEnum x = case x of
        HGBRDisabled -> 0
        HGBREnabled -> 1
        H264GopBReference' name -> (error . showText) $ "Unknown H264GopBReference: " <> original name

-- | Represents the bounds of /known/ $H264GopBReference.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264GopBReference where
    minBound = HGBRDisabled
    maxBound = HGBREnabled

instance Hashable     H264GopBReference
instance NFData       H264GopBReference
instance ToByteString H264GopBReference
instance ToQuery      H264GopBReference
instance ToHeader     H264GopBReference

instance ToJSON H264GopBReference where
    toJSON = toJSONText

instance FromJSON H264GopBReference where
    parseJSON = parseJSONText "H264GopBReference"
