{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.DeliveryStreamType (
  DeliveryStreamType (
    ..
    , DirectPut
    , KinesisStreamAsSource
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeliveryStreamType = DeliveryStreamType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern DirectPut :: DeliveryStreamType
pattern DirectPut = DeliveryStreamType' "DirectPut"

pattern KinesisStreamAsSource :: DeliveryStreamType
pattern KinesisStreamAsSource = DeliveryStreamType' "KinesisStreamAsSource"

{-# COMPLETE
  DirectPut,
  KinesisStreamAsSource,
  DeliveryStreamType' #-}

instance FromText DeliveryStreamType where
    parser = (DeliveryStreamType' . mk) <$> takeText

instance ToText DeliveryStreamType where
    toText (DeliveryStreamType' ci) = original ci

-- | Represents an enum of /known/ $DeliveryStreamType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeliveryStreamType where
    toEnum i = case i of
        0 -> DirectPut
        1 -> KinesisStreamAsSource
        _ -> (error . showText) $ "Unknown index for DeliveryStreamType: " <> toText i
    fromEnum x = case x of
        DirectPut -> 0
        KinesisStreamAsSource -> 1
        DeliveryStreamType' name -> (error . showText) $ "Unknown DeliveryStreamType: " <> original name

-- | Represents the bounds of /known/ $DeliveryStreamType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeliveryStreamType where
    minBound = DirectPut
    maxBound = KinesisStreamAsSource

instance Hashable     DeliveryStreamType
instance NFData       DeliveryStreamType
instance ToByteString DeliveryStreamType
instance ToQuery      DeliveryStreamType
instance ToHeader     DeliveryStreamType

instance ToJSON DeliveryStreamType where
    toJSON = toJSONText

instance FromJSON DeliveryStreamType where
    parseJSON = parseJSONText "DeliveryStreamType"
