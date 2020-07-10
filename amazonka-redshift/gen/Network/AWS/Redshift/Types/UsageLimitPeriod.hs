{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UsageLimitPeriod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.UsageLimitPeriod (
  UsageLimitPeriod (
    ..
    , Daily
    , Monthly
    , Weekly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data UsageLimitPeriod = UsageLimitPeriod' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Daily :: UsageLimitPeriod
pattern Daily = UsageLimitPeriod' "daily"

pattern Monthly :: UsageLimitPeriod
pattern Monthly = UsageLimitPeriod' "monthly"

pattern Weekly :: UsageLimitPeriod
pattern Weekly = UsageLimitPeriod' "weekly"

{-# COMPLETE
  Daily,
  Monthly,
  Weekly,
  UsageLimitPeriod' #-}

instance FromText UsageLimitPeriod where
    parser = (UsageLimitPeriod' . mk) <$> takeText

instance ToText UsageLimitPeriod where
    toText (UsageLimitPeriod' ci) = original ci

-- | Represents an enum of /known/ $UsageLimitPeriod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UsageLimitPeriod where
    toEnum i = case i of
        0 -> Daily
        1 -> Monthly
        2 -> Weekly
        _ -> (error . showText) $ "Unknown index for UsageLimitPeriod: " <> toText i
    fromEnum x = case x of
        Daily -> 0
        Monthly -> 1
        Weekly -> 2
        UsageLimitPeriod' name -> (error . showText) $ "Unknown UsageLimitPeriod: " <> original name

-- | Represents the bounds of /known/ $UsageLimitPeriod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UsageLimitPeriod where
    minBound = Daily
    maxBound = Weekly

instance Hashable     UsageLimitPeriod
instance NFData       UsageLimitPeriod
instance ToByteString UsageLimitPeriod
instance ToQuery      UsageLimitPeriod
instance ToHeader     UsageLimitPeriod

instance FromXML UsageLimitPeriod where
    parseXML = parseXMLText "UsageLimitPeriod"
