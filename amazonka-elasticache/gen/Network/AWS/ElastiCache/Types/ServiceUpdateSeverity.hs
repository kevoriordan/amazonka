{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.ServiceUpdateSeverity (
  ServiceUpdateSeverity (
    ..
    , Critical
    , Important
    , Low
    , Medium
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceUpdateSeverity = ServiceUpdateSeverity' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Critical :: ServiceUpdateSeverity
pattern Critical = ServiceUpdateSeverity' "critical"

pattern Important :: ServiceUpdateSeverity
pattern Important = ServiceUpdateSeverity' "important"

pattern Low :: ServiceUpdateSeverity
pattern Low = ServiceUpdateSeverity' "low"

pattern Medium :: ServiceUpdateSeverity
pattern Medium = ServiceUpdateSeverity' "medium"

{-# COMPLETE
  Critical,
  Important,
  Low,
  Medium,
  ServiceUpdateSeverity' #-}

instance FromText ServiceUpdateSeverity where
    parser = (ServiceUpdateSeverity' . mk) <$> takeText

instance ToText ServiceUpdateSeverity where
    toText (ServiceUpdateSeverity' ci) = original ci

-- | Represents an enum of /known/ $ServiceUpdateSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceUpdateSeverity where
    toEnum i = case i of
        0 -> Critical
        1 -> Important
        2 -> Low
        3 -> Medium
        _ -> (error . showText) $ "Unknown index for ServiceUpdateSeverity: " <> toText i
    fromEnum x = case x of
        Critical -> 0
        Important -> 1
        Low -> 2
        Medium -> 3
        ServiceUpdateSeverity' name -> (error . showText) $ "Unknown ServiceUpdateSeverity: " <> original name

-- | Represents the bounds of /known/ $ServiceUpdateSeverity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceUpdateSeverity where
    minBound = Critical
    maxBound = Medium

instance Hashable     ServiceUpdateSeverity
instance NFData       ServiceUpdateSeverity
instance ToByteString ServiceUpdateSeverity
instance ToQuery      ServiceUpdateSeverity
instance ToHeader     ServiceUpdateSeverity

instance FromXML ServiceUpdateSeverity where
    parseXML = parseXMLText "ServiceUpdateSeverity"
