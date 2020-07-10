{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ConsumerStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.ConsumerStatus (
  ConsumerStatus (
    ..
    , Active
    , Creating
    , Deleting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConsumerStatus = ConsumerStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Active :: ConsumerStatus
pattern Active = ConsumerStatus' "ACTIVE"

pattern Creating :: ConsumerStatus
pattern Creating = ConsumerStatus' "CREATING"

pattern Deleting :: ConsumerStatus
pattern Deleting = ConsumerStatus' "DELETING"

{-# COMPLETE
  Active,
  Creating,
  Deleting,
  ConsumerStatus' #-}

instance FromText ConsumerStatus where
    parser = (ConsumerStatus' . mk) <$> takeText

instance ToText ConsumerStatus where
    toText (ConsumerStatus' ci) = original ci

-- | Represents an enum of /known/ $ConsumerStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConsumerStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Creating
        2 -> Deleting
        _ -> (error . showText) $ "Unknown index for ConsumerStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Creating -> 1
        Deleting -> 2
        ConsumerStatus' name -> (error . showText) $ "Unknown ConsumerStatus: " <> original name

-- | Represents the bounds of /known/ $ConsumerStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConsumerStatus where
    minBound = Active
    maxBound = Deleting

instance Hashable     ConsumerStatus
instance NFData       ConsumerStatus
instance ToByteString ConsumerStatus
instance ToQuery      ConsumerStatus
instance ToHeader     ConsumerStatus

instance FromJSON ConsumerStatus where
    parseJSON = parseJSONText "ConsumerStatus"
