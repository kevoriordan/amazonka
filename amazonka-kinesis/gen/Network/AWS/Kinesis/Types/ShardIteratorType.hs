{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ShardIteratorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.ShardIteratorType (
  ShardIteratorType (
    ..
    , AfterSequenceNumber
    , AtSequenceNumber
    , AtTimestamp
    , Latest
    , TrimHorizon
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ShardIteratorType = ShardIteratorType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern AfterSequenceNumber :: ShardIteratorType
pattern AfterSequenceNumber = ShardIteratorType' "AFTER_SEQUENCE_NUMBER"

pattern AtSequenceNumber :: ShardIteratorType
pattern AtSequenceNumber = ShardIteratorType' "AT_SEQUENCE_NUMBER"

pattern AtTimestamp :: ShardIteratorType
pattern AtTimestamp = ShardIteratorType' "AT_TIMESTAMP"

pattern Latest :: ShardIteratorType
pattern Latest = ShardIteratorType' "LATEST"

pattern TrimHorizon :: ShardIteratorType
pattern TrimHorizon = ShardIteratorType' "TRIM_HORIZON"

{-# COMPLETE
  AfterSequenceNumber,
  AtSequenceNumber,
  AtTimestamp,
  Latest,
  TrimHorizon,
  ShardIteratorType' #-}

instance FromText ShardIteratorType where
    parser = (ShardIteratorType' . mk) <$> takeText

instance ToText ShardIteratorType where
    toText (ShardIteratorType' ci) = original ci

-- | Represents an enum of /known/ $ShardIteratorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ShardIteratorType where
    toEnum i = case i of
        0 -> AfterSequenceNumber
        1 -> AtSequenceNumber
        2 -> AtTimestamp
        3 -> Latest
        4 -> TrimHorizon
        _ -> (error . showText) $ "Unknown index for ShardIteratorType: " <> toText i
    fromEnum x = case x of
        AfterSequenceNumber -> 0
        AtSequenceNumber -> 1
        AtTimestamp -> 2
        Latest -> 3
        TrimHorizon -> 4
        ShardIteratorType' name -> (error . showText) $ "Unknown ShardIteratorType: " <> original name

-- | Represents the bounds of /known/ $ShardIteratorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ShardIteratorType where
    minBound = AfterSequenceNumber
    maxBound = TrimHorizon

instance Hashable     ShardIteratorType
instance NFData       ShardIteratorType
instance ToByteString ShardIteratorType
instance ToQuery      ShardIteratorType
instance ToHeader     ShardIteratorType

instance ToJSON ShardIteratorType where
    toJSON = toJSONText
