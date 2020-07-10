{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditFindingSeverity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditFindingSeverity (
  AuditFindingSeverity (
    ..
    , Critical
    , High
    , Low
    , Medium
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuditFindingSeverity = AuditFindingSeverity' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Critical :: AuditFindingSeverity
pattern Critical = AuditFindingSeverity' "CRITICAL"

pattern High :: AuditFindingSeverity
pattern High = AuditFindingSeverity' "HIGH"

pattern Low :: AuditFindingSeverity
pattern Low = AuditFindingSeverity' "LOW"

pattern Medium :: AuditFindingSeverity
pattern Medium = AuditFindingSeverity' "MEDIUM"

{-# COMPLETE
  Critical,
  High,
  Low,
  Medium,
  AuditFindingSeverity' #-}

instance FromText AuditFindingSeverity where
    parser = (AuditFindingSeverity' . mk) <$> takeText

instance ToText AuditFindingSeverity where
    toText (AuditFindingSeverity' ci) = original ci

-- | Represents an enum of /known/ $AuditFindingSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuditFindingSeverity where
    toEnum i = case i of
        0 -> Critical
        1 -> High
        2 -> Low
        3 -> Medium
        _ -> (error . showText) $ "Unknown index for AuditFindingSeverity: " <> toText i
    fromEnum x = case x of
        Critical -> 0
        High -> 1
        Low -> 2
        Medium -> 3
        AuditFindingSeverity' name -> (error . showText) $ "Unknown AuditFindingSeverity: " <> original name

-- | Represents the bounds of /known/ $AuditFindingSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuditFindingSeverity where
    minBound = Critical
    maxBound = Medium

instance Hashable     AuditFindingSeverity
instance NFData       AuditFindingSeverity
instance ToByteString AuditFindingSeverity
instance ToQuery      AuditFindingSeverity
instance ToHeader     AuditFindingSeverity

instance FromJSON AuditFindingSeverity where
    parseJSON = parseJSONText "AuditFindingSeverity"
