{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.LanguageCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.LanguageCode (
  LanguageCode (
    ..
    , AR
    , DE
    , EN
    , ES
    , FR
    , HI
    , IT
    , JA
    , KO
    , PT
    , ZH
    , ZhTw
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LanguageCode = LanguageCode' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern AR :: LanguageCode
pattern AR = LanguageCode' "ar"

pattern DE :: LanguageCode
pattern DE = LanguageCode' "de"

pattern EN :: LanguageCode
pattern EN = LanguageCode' "en"

pattern ES :: LanguageCode
pattern ES = LanguageCode' "es"

pattern FR :: LanguageCode
pattern FR = LanguageCode' "fr"

pattern HI :: LanguageCode
pattern HI = LanguageCode' "hi"

pattern IT :: LanguageCode
pattern IT = LanguageCode' "it"

pattern JA :: LanguageCode
pattern JA = LanguageCode' "ja"

pattern KO :: LanguageCode
pattern KO = LanguageCode' "ko"

pattern PT :: LanguageCode
pattern PT = LanguageCode' "pt"

pattern ZH :: LanguageCode
pattern ZH = LanguageCode' "zh"

pattern ZhTw :: LanguageCode
pattern ZhTw = LanguageCode' "zh-TW"

{-# COMPLETE
  AR,
  DE,
  EN,
  ES,
  FR,
  HI,
  IT,
  JA,
  KO,
  PT,
  ZH,
  ZhTw,
  LanguageCode' #-}

instance FromText LanguageCode where
    parser = (LanguageCode' . mk) <$> takeText

instance ToText LanguageCode where
    toText (LanguageCode' ci) = original ci

-- | Represents an enum of /known/ $LanguageCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LanguageCode where
    toEnum i = case i of
        0 -> AR
        1 -> DE
        2 -> EN
        3 -> ES
        4 -> FR
        5 -> HI
        6 -> IT
        7 -> JA
        8 -> KO
        9 -> PT
        10 -> ZH
        11 -> ZhTw
        _ -> (error . showText) $ "Unknown index for LanguageCode: " <> toText i
    fromEnum x = case x of
        AR -> 0
        DE -> 1
        EN -> 2
        ES -> 3
        FR -> 4
        HI -> 5
        IT -> 6
        JA -> 7
        KO -> 8
        PT -> 9
        ZH -> 10
        ZhTw -> 11
        LanguageCode' name -> (error . showText) $ "Unknown LanguageCode: " <> original name

-- | Represents the bounds of /known/ $LanguageCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LanguageCode where
    minBound = AR
    maxBound = ZhTw

instance Hashable     LanguageCode
instance NFData       LanguageCode
instance ToByteString LanguageCode
instance ToQuery      LanguageCode
instance ToHeader     LanguageCode

instance ToJSON LanguageCode where
    toJSON = toJSONText

instance FromJSON LanguageCode where
    parseJSON = parseJSONText "LanguageCode"
