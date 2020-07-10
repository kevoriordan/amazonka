{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType (
  HLSFragmentSelectorType (
    ..
    , ProducerTimestamp
    , ServerTimestamp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HLSFragmentSelectorType = HLSFragmentSelectorType' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern ProducerTimestamp :: HLSFragmentSelectorType
pattern ProducerTimestamp = HLSFragmentSelectorType' "PRODUCER_TIMESTAMP"

pattern ServerTimestamp :: HLSFragmentSelectorType
pattern ServerTimestamp = HLSFragmentSelectorType' "SERVER_TIMESTAMP"

{-# COMPLETE
  ProducerTimestamp,
  ServerTimestamp,
  HLSFragmentSelectorType' #-}

instance FromText HLSFragmentSelectorType where
    parser = (HLSFragmentSelectorType' . mk) <$> takeText

instance ToText HLSFragmentSelectorType where
    toText (HLSFragmentSelectorType' ci) = original ci

-- | Represents an enum of /known/ $HLSFragmentSelectorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HLSFragmentSelectorType where
    toEnum i = case i of
        0 -> ProducerTimestamp
        1 -> ServerTimestamp
        _ -> (error . showText) $ "Unknown index for HLSFragmentSelectorType: " <> toText i
    fromEnum x = case x of
        ProducerTimestamp -> 0
        ServerTimestamp -> 1
        HLSFragmentSelectorType' name -> (error . showText) $ "Unknown HLSFragmentSelectorType: " <> original name

-- | Represents the bounds of /known/ $HLSFragmentSelectorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HLSFragmentSelectorType where
    minBound = ProducerTimestamp
    maxBound = ServerTimestamp

instance Hashable     HLSFragmentSelectorType
instance NFData       HLSFragmentSelectorType
instance ToByteString HLSFragmentSelectorType
instance ToQuery      HLSFragmentSelectorType
instance ToHeader     HLSFragmentSelectorType

instance ToJSON HLSFragmentSelectorType where
    toJSON = toJSONText
