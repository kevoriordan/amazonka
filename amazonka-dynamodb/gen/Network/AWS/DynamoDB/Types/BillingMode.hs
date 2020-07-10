{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BillingMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.BillingMode (
  BillingMode (
    ..
    , PayPerRequest
    , Provisioned
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BillingMode = BillingMode' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern PayPerRequest :: BillingMode
pattern PayPerRequest = BillingMode' "PAY_PER_REQUEST"

pattern Provisioned :: BillingMode
pattern Provisioned = BillingMode' "PROVISIONED"

{-# COMPLETE
  PayPerRequest,
  Provisioned,
  BillingMode' #-}

instance FromText BillingMode where
    parser = (BillingMode' . mk) <$> takeText

instance ToText BillingMode where
    toText (BillingMode' ci) = original ci

-- | Represents an enum of /known/ $BillingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BillingMode where
    toEnum i = case i of
        0 -> PayPerRequest
        1 -> Provisioned
        _ -> (error . showText) $ "Unknown index for BillingMode: " <> toText i
    fromEnum x = case x of
        PayPerRequest -> 0
        Provisioned -> 1
        BillingMode' name -> (error . showText) $ "Unknown BillingMode: " <> original name

-- | Represents the bounds of /known/ $BillingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BillingMode where
    minBound = PayPerRequest
    maxBound = Provisioned

instance Hashable     BillingMode
instance NFData       BillingMode
instance ToByteString BillingMode
instance ToQuery      BillingMode
instance ToHeader     BillingMode

instance ToJSON BillingMode where
    toJSON = toJSONText

instance FromJSON BillingMode where
    parseJSON = parseJSONText "BillingMode"
