{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UsageReportSchedule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.UsageReportSchedule (
  UsageReportSchedule (
    ..
    , Daily
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UsageReportSchedule = UsageReportSchedule' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Daily :: UsageReportSchedule
pattern Daily = UsageReportSchedule' "DAILY"

{-# COMPLETE
  Daily,
  UsageReportSchedule' #-}

instance FromText UsageReportSchedule where
    parser = (UsageReportSchedule' . mk) <$> takeText

instance ToText UsageReportSchedule where
    toText (UsageReportSchedule' ci) = original ci

-- | Represents an enum of /known/ $UsageReportSchedule.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UsageReportSchedule where
    toEnum i = case i of
        0 -> Daily
        _ -> (error . showText) $ "Unknown index for UsageReportSchedule: " <> toText i
    fromEnum x = case x of
        Daily -> 0
        UsageReportSchedule' name -> (error . showText) $ "Unknown UsageReportSchedule: " <> original name

-- | Represents the bounds of /known/ $UsageReportSchedule.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UsageReportSchedule where
    minBound = Daily
    maxBound = Daily

instance Hashable     UsageReportSchedule
instance NFData       UsageReportSchedule
instance ToByteString UsageReportSchedule
instance ToQuery      UsageReportSchedule
instance ToHeader     UsageReportSchedule

instance FromJSON UsageReportSchedule where
    parseJSON = parseJSONText "UsageReportSchedule"
