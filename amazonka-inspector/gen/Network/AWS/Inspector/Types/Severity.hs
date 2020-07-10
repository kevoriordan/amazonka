{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Severity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.Severity (
  Severity (
    ..
    , High
    , Informational
    , Low
    , Medium
    , Undefined
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Severity = Severity' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern High :: Severity
pattern High = Severity' "High"

pattern Informational :: Severity
pattern Informational = Severity' "Informational"

pattern Low :: Severity
pattern Low = Severity' "Low"

pattern Medium :: Severity
pattern Medium = Severity' "Medium"

pattern Undefined :: Severity
pattern Undefined = Severity' "Undefined"

{-# COMPLETE
  High,
  Informational,
  Low,
  Medium,
  Undefined,
  Severity' #-}

instance FromText Severity where
    parser = (Severity' . mk) <$> takeText

instance ToText Severity where
    toText (Severity' ci) = original ci

-- | Represents an enum of /known/ $Severity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Severity where
    toEnum i = case i of
        0 -> High
        1 -> Informational
        2 -> Low
        3 -> Medium
        4 -> Undefined
        _ -> (error . showText) $ "Unknown index for Severity: " <> toText i
    fromEnum x = case x of
        High -> 0
        Informational -> 1
        Low -> 2
        Medium -> 3
        Undefined -> 4
        Severity' name -> (error . showText) $ "Unknown Severity: " <> original name

-- | Represents the bounds of /known/ $Severity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Severity where
    minBound = High
    maxBound = Undefined

instance Hashable     Severity
instance NFData       Severity
instance ToByteString Severity
instance ToQuery      Severity
instance ToHeader     Severity

instance ToJSON Severity where
    toJSON = toJSONText

instance FromJSON Severity where
    parseJSON = parseJSONText "Severity"
