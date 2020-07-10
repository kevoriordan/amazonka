{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TopicRuleDestinationStatus (
  TopicRuleDestinationStatus (
    ..
    , TRDSDisabled
    , TRDSEnabled
    , TRDSError'
    , TRDSInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TopicRuleDestinationStatus = TopicRuleDestinationStatus' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern TRDSDisabled :: TopicRuleDestinationStatus
pattern TRDSDisabled = TopicRuleDestinationStatus' "DISABLED"

pattern TRDSEnabled :: TopicRuleDestinationStatus
pattern TRDSEnabled = TopicRuleDestinationStatus' "ENABLED"

pattern TRDSError' :: TopicRuleDestinationStatus
pattern TRDSError' = TopicRuleDestinationStatus' "ERROR"

pattern TRDSInProgress :: TopicRuleDestinationStatus
pattern TRDSInProgress = TopicRuleDestinationStatus' "IN_PROGRESS"

{-# COMPLETE
  TRDSDisabled,
  TRDSEnabled,
  TRDSError',
  TRDSInProgress,
  TopicRuleDestinationStatus' #-}

instance FromText TopicRuleDestinationStatus where
    parser = (TopicRuleDestinationStatus' . mk) <$> takeText

instance ToText TopicRuleDestinationStatus where
    toText (TopicRuleDestinationStatus' ci) = original ci

-- | Represents an enum of /known/ $TopicRuleDestinationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TopicRuleDestinationStatus where
    toEnum i = case i of
        0 -> TRDSDisabled
        1 -> TRDSEnabled
        2 -> TRDSError'
        3 -> TRDSInProgress
        _ -> (error . showText) $ "Unknown index for TopicRuleDestinationStatus: " <> toText i
    fromEnum x = case x of
        TRDSDisabled -> 0
        TRDSEnabled -> 1
        TRDSError' -> 2
        TRDSInProgress -> 3
        TopicRuleDestinationStatus' name -> (error . showText) $ "Unknown TopicRuleDestinationStatus: " <> original name

-- | Represents the bounds of /known/ $TopicRuleDestinationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TopicRuleDestinationStatus where
    minBound = TRDSDisabled
    maxBound = TRDSInProgress

instance Hashable     TopicRuleDestinationStatus
instance NFData       TopicRuleDestinationStatus
instance ToByteString TopicRuleDestinationStatus
instance ToQuery      TopicRuleDestinationStatus
instance ToHeader     TopicRuleDestinationStatus

instance ToJSON TopicRuleDestinationStatus where
    toJSON = toJSONText

instance FromJSON TopicRuleDestinationStatus where
    parseJSON = parseJSONText "TopicRuleDestinationStatus"
