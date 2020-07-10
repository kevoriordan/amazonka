{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.TemplateType (
  TemplateType (
    ..
    , TTEmail
    , TTPush
    , TTSms
    , TTVoice
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TemplateType = TemplateType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern TTEmail :: TemplateType
pattern TTEmail = TemplateType' "EMAIL"

pattern TTPush :: TemplateType
pattern TTPush = TemplateType' "PUSH"

pattern TTSms :: TemplateType
pattern TTSms = TemplateType' "SMS"

pattern TTVoice :: TemplateType
pattern TTVoice = TemplateType' "VOICE"

{-# COMPLETE
  TTEmail,
  TTPush,
  TTSms,
  TTVoice,
  TemplateType' #-}

instance FromText TemplateType where
    parser = (TemplateType' . mk) <$> takeText

instance ToText TemplateType where
    toText (TemplateType' ci) = original ci

-- | Represents an enum of /known/ $TemplateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TemplateType where
    toEnum i = case i of
        0 -> TTEmail
        1 -> TTPush
        2 -> TTSms
        3 -> TTVoice
        _ -> (error . showText) $ "Unknown index for TemplateType: " <> toText i
    fromEnum x = case x of
        TTEmail -> 0
        TTPush -> 1
        TTSms -> 2
        TTVoice -> 3
        TemplateType' name -> (error . showText) $ "Unknown TemplateType: " <> original name

-- | Represents the bounds of /known/ $TemplateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TemplateType where
    minBound = TTEmail
    maxBound = TTVoice

instance Hashable     TemplateType
instance NFData       TemplateType
instance ToByteString TemplateType
instance ToQuery      TemplateType
instance ToHeader     TemplateType

instance FromJSON TemplateType where
    parseJSON = parseJSONText "TemplateType"
