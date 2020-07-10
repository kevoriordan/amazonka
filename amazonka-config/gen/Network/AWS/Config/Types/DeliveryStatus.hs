{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.DeliveryStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.DeliveryStatus (
  DeliveryStatus (
    ..
    , DSFailure
    , DSNotApplicable
    , DSSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeliveryStatus = DeliveryStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern DSFailure :: DeliveryStatus
pattern DSFailure = DeliveryStatus' "Failure"

pattern DSNotApplicable :: DeliveryStatus
pattern DSNotApplicable = DeliveryStatus' "Not_Applicable"

pattern DSSuccess :: DeliveryStatus
pattern DSSuccess = DeliveryStatus' "Success"

{-# COMPLETE
  DSFailure,
  DSNotApplicable,
  DSSuccess,
  DeliveryStatus' #-}

instance FromText DeliveryStatus where
    parser = (DeliveryStatus' . mk) <$> takeText

instance ToText DeliveryStatus where
    toText (DeliveryStatus' ci) = original ci

-- | Represents an enum of /known/ $DeliveryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeliveryStatus where
    toEnum i = case i of
        0 -> DSFailure
        1 -> DSNotApplicable
        2 -> DSSuccess
        _ -> (error . showText) $ "Unknown index for DeliveryStatus: " <> toText i
    fromEnum x = case x of
        DSFailure -> 0
        DSNotApplicable -> 1
        DSSuccess -> 2
        DeliveryStatus' name -> (error . showText) $ "Unknown DeliveryStatus: " <> original name

-- | Represents the bounds of /known/ $DeliveryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeliveryStatus where
    minBound = DSFailure
    maxBound = DSSuccess

instance Hashable     DeliveryStatus
instance NFData       DeliveryStatus
instance ToByteString DeliveryStatus
instance ToQuery      DeliveryStatus
instance ToHeader     DeliveryStatus

instance FromJSON DeliveryStatus where
    parseJSON = parseJSONText "DeliveryStatus"
