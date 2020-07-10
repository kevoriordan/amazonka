{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior (
  Scte35SpliceInsertWebDeliveryAllowedBehavior (
    ..
    , SSIWDABFollow
    , SSIWDABIgnore
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Scte35 Splice Insert Web Delivery Allowed Behavior
data Scte35SpliceInsertWebDeliveryAllowedBehavior = Scte35SpliceInsertWebDeliveryAllowedBehavior' (CI
                                                                                                     Text)
                                                      deriving (Eq, Ord, Read,
                                                                Show, Data,
                                                                Typeable,
                                                                Generic)

pattern SSIWDABFollow :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern SSIWDABFollow = Scte35SpliceInsertWebDeliveryAllowedBehavior' "FOLLOW"

pattern SSIWDABIgnore :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern SSIWDABIgnore = Scte35SpliceInsertWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE
  SSIWDABFollow,
  SSIWDABIgnore,
  Scte35SpliceInsertWebDeliveryAllowedBehavior' #-}

instance FromText Scte35SpliceInsertWebDeliveryAllowedBehavior where
    parser = (Scte35SpliceInsertWebDeliveryAllowedBehavior' . mk) <$> takeText

instance ToText Scte35SpliceInsertWebDeliveryAllowedBehavior where
    toText (Scte35SpliceInsertWebDeliveryAllowedBehavior' ci) = original ci

-- | Represents an enum of /known/ $Scte35SpliceInsertWebDeliveryAllowedBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Scte35SpliceInsertWebDeliveryAllowedBehavior where
    toEnum i = case i of
        0 -> SSIWDABFollow
        1 -> SSIWDABIgnore
        _ -> (error . showText) $ "Unknown index for Scte35SpliceInsertWebDeliveryAllowedBehavior: " <> toText i
    fromEnum x = case x of
        SSIWDABFollow -> 0
        SSIWDABIgnore -> 1
        Scte35SpliceInsertWebDeliveryAllowedBehavior' name -> (error . showText) $ "Unknown Scte35SpliceInsertWebDeliveryAllowedBehavior: " <> original name

-- | Represents the bounds of /known/ $Scte35SpliceInsertWebDeliveryAllowedBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Scte35SpliceInsertWebDeliveryAllowedBehavior where
    minBound = SSIWDABFollow
    maxBound = SSIWDABIgnore

instance Hashable     Scte35SpliceInsertWebDeliveryAllowedBehavior
instance NFData       Scte35SpliceInsertWebDeliveryAllowedBehavior
instance ToByteString Scte35SpliceInsertWebDeliveryAllowedBehavior
instance ToQuery      Scte35SpliceInsertWebDeliveryAllowedBehavior
instance ToHeader     Scte35SpliceInsertWebDeliveryAllowedBehavior

instance ToJSON Scte35SpliceInsertWebDeliveryAllowedBehavior where
    toJSON = toJSONText

instance FromJSON Scte35SpliceInsertWebDeliveryAllowedBehavior where
    parseJSON = parseJSONText "Scte35SpliceInsertWebDeliveryAllowedBehavior"
