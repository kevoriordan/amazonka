{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.LocaleType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.LocaleType (
  LocaleType (
    ..
    , DE
    , Default
    , EN
    , ES
    , FR
    , JA
    , KO
    , PtBr
    , RU
    , ZhCn
    , ZhTw
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LocaleType = LocaleType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern DE :: LocaleType
pattern DE = LocaleType' "de"

pattern Default :: LocaleType
pattern Default = LocaleType' "default"

pattern EN :: LocaleType
pattern EN = LocaleType' "en"

pattern ES :: LocaleType
pattern ES = LocaleType' "es"

pattern FR :: LocaleType
pattern FR = LocaleType' "fr"

pattern JA :: LocaleType
pattern JA = LocaleType' "ja"

pattern KO :: LocaleType
pattern KO = LocaleType' "ko"

pattern PtBr :: LocaleType
pattern PtBr = LocaleType' "pt_BR"

pattern RU :: LocaleType
pattern RU = LocaleType' "ru"

pattern ZhCn :: LocaleType
pattern ZhCn = LocaleType' "zh_CN"

pattern ZhTw :: LocaleType
pattern ZhTw = LocaleType' "zh_TW"

{-# COMPLETE
  DE,
  Default,
  EN,
  ES,
  FR,
  JA,
  KO,
  PtBr,
  RU,
  ZhCn,
  ZhTw,
  LocaleType' #-}

instance FromText LocaleType where
    parser = (LocaleType' . mk) <$> takeText

instance ToText LocaleType where
    toText (LocaleType' ci) = original ci

-- | Represents an enum of /known/ $LocaleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LocaleType where
    toEnum i = case i of
        0 -> DE
        1 -> Default
        2 -> EN
        3 -> ES
        4 -> FR
        5 -> JA
        6 -> KO
        7 -> PtBr
        8 -> RU
        9 -> ZhCn
        10 -> ZhTw
        _ -> (error . showText) $ "Unknown index for LocaleType: " <> toText i
    fromEnum x = case x of
        DE -> 0
        Default -> 1
        EN -> 2
        ES -> 3
        FR -> 4
        JA -> 5
        KO -> 6
        PtBr -> 7
        RU -> 8
        ZhCn -> 9
        ZhTw -> 10
        LocaleType' name -> (error . showText) $ "Unknown LocaleType: " <> original name

-- | Represents the bounds of /known/ $LocaleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LocaleType where
    minBound = DE
    maxBound = ZhTw

instance Hashable     LocaleType
instance NFData       LocaleType
instance ToByteString LocaleType
instance ToQuery      LocaleType
instance ToHeader     LocaleType

instance ToJSON LocaleType where
    toJSON = toJSONText

instance FromJSON LocaleType where
    parseJSON = parseJSONText "LocaleType"
