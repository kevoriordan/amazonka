{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AlarmType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.AlarmType (
  AlarmType (
    ..
    , CompositeAlarm
    , MetricAlarm
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AlarmType = AlarmType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern CompositeAlarm :: AlarmType
pattern CompositeAlarm = AlarmType' "CompositeAlarm"

pattern MetricAlarm :: AlarmType
pattern MetricAlarm = AlarmType' "MetricAlarm"

{-# COMPLETE
  CompositeAlarm,
  MetricAlarm,
  AlarmType' #-}

instance FromText AlarmType where
    parser = (AlarmType' . mk) <$> takeText

instance ToText AlarmType where
    toText (AlarmType' ci) = original ci

-- | Represents an enum of /known/ $AlarmType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AlarmType where
    toEnum i = case i of
        0 -> CompositeAlarm
        1 -> MetricAlarm
        _ -> (error . showText) $ "Unknown index for AlarmType: " <> toText i
    fromEnum x = case x of
        CompositeAlarm -> 0
        MetricAlarm -> 1
        AlarmType' name -> (error . showText) $ "Unknown AlarmType: " <> original name

-- | Represents the bounds of /known/ $AlarmType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AlarmType where
    minBound = CompositeAlarm
    maxBound = MetricAlarm

instance Hashable     AlarmType
instance NFData       AlarmType
instance ToByteString AlarmType
instance ToQuery      AlarmType
instance ToHeader     AlarmType

instance FromXML AlarmType where
    parseXML = parseXMLText "AlarmType"
