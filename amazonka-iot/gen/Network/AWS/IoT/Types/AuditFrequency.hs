{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditFrequency
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditFrequency (
  AuditFrequency (
    ..
    , Biweekly
    , Daily
    , Monthly
    , Weekly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuditFrequency = AuditFrequency' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Biweekly :: AuditFrequency
pattern Biweekly = AuditFrequency' "BIWEEKLY"

pattern Daily :: AuditFrequency
pattern Daily = AuditFrequency' "DAILY"

pattern Monthly :: AuditFrequency
pattern Monthly = AuditFrequency' "MONTHLY"

pattern Weekly :: AuditFrequency
pattern Weekly = AuditFrequency' "WEEKLY"

{-# COMPLETE
  Biweekly,
  Daily,
  Monthly,
  Weekly,
  AuditFrequency' #-}

instance FromText AuditFrequency where
    parser = (AuditFrequency' . mk) <$> takeText

instance ToText AuditFrequency where
    toText (AuditFrequency' ci) = original ci

-- | Represents an enum of /known/ $AuditFrequency.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuditFrequency where
    toEnum i = case i of
        0 -> Biweekly
        1 -> Daily
        2 -> Monthly
        3 -> Weekly
        _ -> (error . showText) $ "Unknown index for AuditFrequency: " <> toText i
    fromEnum x = case x of
        Biweekly -> 0
        Daily -> 1
        Monthly -> 2
        Weekly -> 3
        AuditFrequency' name -> (error . showText) $ "Unknown AuditFrequency: " <> original name

-- | Represents the bounds of /known/ $AuditFrequency.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuditFrequency where
    minBound = Biweekly
    maxBound = Weekly

instance Hashable     AuditFrequency
instance NFData       AuditFrequency
instance ToByteString AuditFrequency
instance ToQuery      AuditFrequency
instance ToHeader     AuditFrequency

instance ToJSON AuditFrequency where
    toJSON = toJSONText

instance FromJSON AuditFrequency where
    parseJSON = parseJSONText "AuditFrequency"
