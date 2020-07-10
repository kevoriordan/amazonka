{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APICacheType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.APICacheType (
  APICacheType (
    ..
    , R42XLARGE
    , R44XLARGE
    , R48XLARGE
    , R4Large
    , R4XLarge
    , T2Medium
    , T2Small
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data APICacheType = APICacheType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern R42XLARGE :: APICacheType
pattern R42XLARGE = APICacheType' "R4_2XLARGE"

pattern R44XLARGE :: APICacheType
pattern R44XLARGE = APICacheType' "R4_4XLARGE"

pattern R48XLARGE :: APICacheType
pattern R48XLARGE = APICacheType' "R4_8XLARGE"

pattern R4Large :: APICacheType
pattern R4Large = APICacheType' "R4_LARGE"

pattern R4XLarge :: APICacheType
pattern R4XLarge = APICacheType' "R4_XLARGE"

pattern T2Medium :: APICacheType
pattern T2Medium = APICacheType' "T2_MEDIUM"

pattern T2Small :: APICacheType
pattern T2Small = APICacheType' "T2_SMALL"

{-# COMPLETE
  R42XLARGE,
  R44XLARGE,
  R48XLARGE,
  R4Large,
  R4XLarge,
  T2Medium,
  T2Small,
  APICacheType' #-}

instance FromText APICacheType where
    parser = (APICacheType' . mk) <$> takeText

instance ToText APICacheType where
    toText (APICacheType' ci) = original ci

-- | Represents an enum of /known/ $APICacheType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum APICacheType where
    toEnum i = case i of
        0 -> R42XLARGE
        1 -> R44XLARGE
        2 -> R48XLARGE
        3 -> R4Large
        4 -> R4XLarge
        5 -> T2Medium
        6 -> T2Small
        _ -> (error . showText) $ "Unknown index for APICacheType: " <> toText i
    fromEnum x = case x of
        R42XLARGE -> 0
        R44XLARGE -> 1
        R48XLARGE -> 2
        R4Large -> 3
        R4XLarge -> 4
        T2Medium -> 5
        T2Small -> 6
        APICacheType' name -> (error . showText) $ "Unknown APICacheType: " <> original name

-- | Represents the bounds of /known/ $APICacheType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded APICacheType where
    minBound = R42XLARGE
    maxBound = T2Small

instance Hashable     APICacheType
instance NFData       APICacheType
instance ToByteString APICacheType
instance ToQuery      APICacheType
instance ToHeader     APICacheType

instance ToJSON APICacheType where
    toJSON = toJSONText

instance FromJSON APICacheType where
    parseJSON = parseJSONText "APICacheType"
