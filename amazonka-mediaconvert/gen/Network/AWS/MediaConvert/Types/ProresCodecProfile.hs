{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresCodecProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ProresCodecProfile (
  ProresCodecProfile (
    ..
    , AppleProres422
    , AppleProres422Hq
    , AppleProres422LT
    , AppleProres422Proxy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
data ProresCodecProfile = ProresCodecProfile' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern AppleProres422 :: ProresCodecProfile
pattern AppleProres422 = ProresCodecProfile' "APPLE_PRORES_422"

pattern AppleProres422Hq :: ProresCodecProfile
pattern AppleProres422Hq = ProresCodecProfile' "APPLE_PRORES_422_HQ"

pattern AppleProres422LT :: ProresCodecProfile
pattern AppleProres422LT = ProresCodecProfile' "APPLE_PRORES_422_LT"

pattern AppleProres422Proxy :: ProresCodecProfile
pattern AppleProres422Proxy = ProresCodecProfile' "APPLE_PRORES_422_PROXY"

{-# COMPLETE
  AppleProres422,
  AppleProres422Hq,
  AppleProres422LT,
  AppleProres422Proxy,
  ProresCodecProfile' #-}

instance FromText ProresCodecProfile where
    parser = (ProresCodecProfile' . mk) <$> takeText

instance ToText ProresCodecProfile where
    toText (ProresCodecProfile' ci) = original ci

-- | Represents an enum of /known/ $ProresCodecProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProresCodecProfile where
    toEnum i = case i of
        0 -> AppleProres422
        1 -> AppleProres422Hq
        2 -> AppleProres422LT
        3 -> AppleProres422Proxy
        _ -> (error . showText) $ "Unknown index for ProresCodecProfile: " <> toText i
    fromEnum x = case x of
        AppleProres422 -> 0
        AppleProres422Hq -> 1
        AppleProres422LT -> 2
        AppleProres422Proxy -> 3
        ProresCodecProfile' name -> (error . showText) $ "Unknown ProresCodecProfile: " <> original name

-- | Represents the bounds of /known/ $ProresCodecProfile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProresCodecProfile where
    minBound = AppleProres422
    maxBound = AppleProres422Proxy

instance Hashable     ProresCodecProfile
instance NFData       ProresCodecProfile
instance ToByteString ProresCodecProfile
instance ToQuery      ProresCodecProfile
instance ToHeader     ProresCodecProfile

instance ToJSON ProresCodecProfile where
    toJSON = toJSONText

instance FromJSON ProresCodecProfile where
    parseJSON = parseJSONText "ProresCodecProfile"
