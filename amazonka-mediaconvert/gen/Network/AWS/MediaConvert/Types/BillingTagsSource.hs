{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BillingTagsSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.BillingTagsSource (
  BillingTagsSource (
    ..
    , Job
    , JobTemplate
    , Preset
    , Queue
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up.
data BillingTagsSource = BillingTagsSource' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Job :: BillingTagsSource
pattern Job = BillingTagsSource' "JOB"

pattern JobTemplate :: BillingTagsSource
pattern JobTemplate = BillingTagsSource' "JOB_TEMPLATE"

pattern Preset :: BillingTagsSource
pattern Preset = BillingTagsSource' "PRESET"

pattern Queue :: BillingTagsSource
pattern Queue = BillingTagsSource' "QUEUE"

{-# COMPLETE
  Job,
  JobTemplate,
  Preset,
  Queue,
  BillingTagsSource' #-}

instance FromText BillingTagsSource where
    parser = (BillingTagsSource' . mk) <$> takeText

instance ToText BillingTagsSource where
    toText (BillingTagsSource' ci) = original ci

-- | Represents an enum of /known/ $BillingTagsSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BillingTagsSource where
    toEnum i = case i of
        0 -> Job
        1 -> JobTemplate
        2 -> Preset
        3 -> Queue
        _ -> (error . showText) $ "Unknown index for BillingTagsSource: " <> toText i
    fromEnum x = case x of
        Job -> 0
        JobTemplate -> 1
        Preset -> 2
        Queue -> 3
        BillingTagsSource' name -> (error . showText) $ "Unknown BillingTagsSource: " <> original name

-- | Represents the bounds of /known/ $BillingTagsSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BillingTagsSource where
    minBound = Job
    maxBound = Queue

instance Hashable     BillingTagsSource
instance NFData       BillingTagsSource
instance ToByteString BillingTagsSource
instance ToQuery      BillingTagsSource
instance ToHeader     BillingTagsSource

instance ToJSON BillingTagsSource where
    toJSON = toJSONText

instance FromJSON BillingTagsSource where
    parseJSON = parseJSONText "BillingTagsSource"
