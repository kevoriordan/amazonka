{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SyntaxLanguageCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.SyntaxLanguageCode (
  SyntaxLanguageCode (
    ..
    , SLCDE
    , SLCEN
    , SLCES
    , SLCFR
    , SLCIT
    , SLCPT
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SyntaxLanguageCode = SyntaxLanguageCode' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern SLCDE :: SyntaxLanguageCode
pattern SLCDE = SyntaxLanguageCode' "de"

pattern SLCEN :: SyntaxLanguageCode
pattern SLCEN = SyntaxLanguageCode' "en"

pattern SLCES :: SyntaxLanguageCode
pattern SLCES = SyntaxLanguageCode' "es"

pattern SLCFR :: SyntaxLanguageCode
pattern SLCFR = SyntaxLanguageCode' "fr"

pattern SLCIT :: SyntaxLanguageCode
pattern SLCIT = SyntaxLanguageCode' "it"

pattern SLCPT :: SyntaxLanguageCode
pattern SLCPT = SyntaxLanguageCode' "pt"

{-# COMPLETE
  SLCDE,
  SLCEN,
  SLCES,
  SLCFR,
  SLCIT,
  SLCPT,
  SyntaxLanguageCode' #-}

instance FromText SyntaxLanguageCode where
    parser = (SyntaxLanguageCode' . mk) <$> takeText

instance ToText SyntaxLanguageCode where
    toText (SyntaxLanguageCode' ci) = original ci

-- | Represents an enum of /known/ $SyntaxLanguageCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SyntaxLanguageCode where
    toEnum i = case i of
        0 -> SLCDE
        1 -> SLCEN
        2 -> SLCES
        3 -> SLCFR
        4 -> SLCIT
        5 -> SLCPT
        _ -> (error . showText) $ "Unknown index for SyntaxLanguageCode: " <> toText i
    fromEnum x = case x of
        SLCDE -> 0
        SLCEN -> 1
        SLCES -> 2
        SLCFR -> 3
        SLCIT -> 4
        SLCPT -> 5
        SyntaxLanguageCode' name -> (error . showText) $ "Unknown SyntaxLanguageCode: " <> original name

-- | Represents the bounds of /known/ $SyntaxLanguageCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SyntaxLanguageCode where
    minBound = SLCDE
    maxBound = SLCPT

instance Hashable     SyntaxLanguageCode
instance NFData       SyntaxLanguageCode
instance ToByteString SyntaxLanguageCode
instance ToQuery      SyntaxLanguageCode
instance ToHeader     SyntaxLanguageCode

instance ToJSON SyntaxLanguageCode where
    toJSON = toJSONText
