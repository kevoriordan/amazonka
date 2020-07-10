{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ExportType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.ExportType (
  ExportType (
    ..
    , AlexaSkillsKit
    , Lex
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExportType = ExportType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern AlexaSkillsKit :: ExportType
pattern AlexaSkillsKit = ExportType' "ALEXA_SKILLS_KIT"

pattern Lex :: ExportType
pattern Lex = ExportType' "LEX"

{-# COMPLETE
  AlexaSkillsKit,
  Lex,
  ExportType' #-}

instance FromText ExportType where
    parser = (ExportType' . mk) <$> takeText

instance ToText ExportType where
    toText (ExportType' ci) = original ci

-- | Represents an enum of /known/ $ExportType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExportType where
    toEnum i = case i of
        0 -> AlexaSkillsKit
        1 -> Lex
        _ -> (error . showText) $ "Unknown index for ExportType: " <> toText i
    fromEnum x = case x of
        AlexaSkillsKit -> 0
        Lex -> 1
        ExportType' name -> (error . showText) $ "Unknown ExportType: " <> original name

-- | Represents the bounds of /known/ $ExportType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExportType where
    minBound = AlexaSkillsKit
    maxBound = Lex

instance Hashable     ExportType
instance NFData       ExportType
instance ToByteString ExportType
instance ToQuery      ExportType
instance ToHeader     ExportType

instance ToJSON ExportType where
    toJSON = toJSONText

instance FromJSON ExportType where
    parseJSON = parseJSONText "ExportType"
