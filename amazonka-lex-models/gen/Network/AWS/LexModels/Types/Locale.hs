{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Locale
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Locale (
  Locale (
    ..
    , DeDe
    , EnGb
    , EnUs
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Locale = Locale' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern DeDe :: Locale
pattern DeDe = Locale' "de-DE"

pattern EnGb :: Locale
pattern EnGb = Locale' "en-GB"

pattern EnUs :: Locale
pattern EnUs = Locale' "en-US"

{-# COMPLETE
  DeDe,
  EnGb,
  EnUs,
  Locale' #-}

instance FromText Locale where
    parser = (Locale' . mk) <$> takeText

instance ToText Locale where
    toText (Locale' ci) = original ci

-- | Represents an enum of /known/ $Locale.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Locale where
    toEnum i = case i of
        0 -> DeDe
        1 -> EnGb
        2 -> EnUs
        _ -> (error . showText) $ "Unknown index for Locale: " <> toText i
    fromEnum x = case x of
        DeDe -> 0
        EnGb -> 1
        EnUs -> 2
        Locale' name -> (error . showText) $ "Unknown Locale: " <> original name

-- | Represents the bounds of /known/ $Locale.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Locale where
    minBound = DeDe
    maxBound = EnUs

instance Hashable     Locale
instance NFData       Locale
instance ToByteString Locale
instance ToQuery      Locale
instance ToHeader     Locale

instance ToJSON Locale where
    toJSON = toJSONText

instance FromJSON Locale where
    parseJSON = parseJSONText "Locale"
