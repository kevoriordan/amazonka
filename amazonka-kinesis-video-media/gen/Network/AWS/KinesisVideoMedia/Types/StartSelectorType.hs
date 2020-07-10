{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types.StartSelectorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoMedia.Types.StartSelectorType (
  StartSelectorType (
    ..
    , ContinuationToken
    , Earliest
    , FragmentNumber
    , Now
    , ProducerTimestamp
    , ServerTimestamp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StartSelectorType = StartSelectorType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ContinuationToken :: StartSelectorType
pattern ContinuationToken = StartSelectorType' "CONTINUATION_TOKEN"

pattern Earliest :: StartSelectorType
pattern Earliest = StartSelectorType' "EARLIEST"

pattern FragmentNumber :: StartSelectorType
pattern FragmentNumber = StartSelectorType' "FRAGMENT_NUMBER"

pattern Now :: StartSelectorType
pattern Now = StartSelectorType' "NOW"

pattern ProducerTimestamp :: StartSelectorType
pattern ProducerTimestamp = StartSelectorType' "PRODUCER_TIMESTAMP"

pattern ServerTimestamp :: StartSelectorType
pattern ServerTimestamp = StartSelectorType' "SERVER_TIMESTAMP"

{-# COMPLETE
  ContinuationToken,
  Earliest,
  FragmentNumber,
  Now,
  ProducerTimestamp,
  ServerTimestamp,
  StartSelectorType' #-}

instance FromText StartSelectorType where
    parser = (StartSelectorType' . mk) <$> takeText

instance ToText StartSelectorType where
    toText (StartSelectorType' ci) = original ci

-- | Represents an enum of /known/ $StartSelectorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StartSelectorType where
    toEnum i = case i of
        0 -> ContinuationToken
        1 -> Earliest
        2 -> FragmentNumber
        3 -> Now
        4 -> ProducerTimestamp
        5 -> ServerTimestamp
        _ -> (error . showText) $ "Unknown index for StartSelectorType: " <> toText i
    fromEnum x = case x of
        ContinuationToken -> 0
        Earliest -> 1
        FragmentNumber -> 2
        Now -> 3
        ProducerTimestamp -> 4
        ServerTimestamp -> 5
        StartSelectorType' name -> (error . showText) $ "Unknown StartSelectorType: " <> original name

-- | Represents the bounds of /known/ $StartSelectorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StartSelectorType where
    minBound = ContinuationToken
    maxBound = ServerTimestamp

instance Hashable     StartSelectorType
instance NFData       StartSelectorType
instance ToByteString StartSelectorType
instance ToQuery      StartSelectorType
instance ToHeader     StartSelectorType

instance ToJSON StartSelectorType where
    toJSON = toJSONText
