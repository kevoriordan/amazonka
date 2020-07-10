{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingTransactionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.OfferingTransactionType (
  OfferingTransactionType (
    ..
    , Purchase
    , Renew
    , System
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OfferingTransactionType = OfferingTransactionType' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Purchase :: OfferingTransactionType
pattern Purchase = OfferingTransactionType' "PURCHASE"

pattern Renew :: OfferingTransactionType
pattern Renew = OfferingTransactionType' "RENEW"

pattern System :: OfferingTransactionType
pattern System = OfferingTransactionType' "SYSTEM"

{-# COMPLETE
  Purchase,
  Renew,
  System,
  OfferingTransactionType' #-}

instance FromText OfferingTransactionType where
    parser = (OfferingTransactionType' . mk) <$> takeText

instance ToText OfferingTransactionType where
    toText (OfferingTransactionType' ci) = original ci

-- | Represents an enum of /known/ $OfferingTransactionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OfferingTransactionType where
    toEnum i = case i of
        0 -> Purchase
        1 -> Renew
        2 -> System
        _ -> (error . showText) $ "Unknown index for OfferingTransactionType: " <> toText i
    fromEnum x = case x of
        Purchase -> 0
        Renew -> 1
        System -> 2
        OfferingTransactionType' name -> (error . showText) $ "Unknown OfferingTransactionType: " <> original name

-- | Represents the bounds of /known/ $OfferingTransactionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OfferingTransactionType where
    minBound = Purchase
    maxBound = System

instance Hashable     OfferingTransactionType
instance NFData       OfferingTransactionType
instance ToByteString OfferingTransactionType
instance ToQuery      OfferingTransactionType
instance ToHeader     OfferingTransactionType

instance FromJSON OfferingTransactionType where
    parseJSON = parseJSONText "OfferingTransactionType"
