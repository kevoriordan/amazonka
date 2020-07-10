{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ProductCodeValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ProductCodeValues (
  ProductCodeValues (
    ..
    , Devpay
    , Marketplace
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ProductCodeValues = ProductCodeValues' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Devpay :: ProductCodeValues
pattern Devpay = ProductCodeValues' "devpay"

pattern Marketplace :: ProductCodeValues
pattern Marketplace = ProductCodeValues' "marketplace"

{-# COMPLETE
  Devpay,
  Marketplace,
  ProductCodeValues' #-}

instance FromText ProductCodeValues where
    parser = (ProductCodeValues' . mk) <$> takeText

instance ToText ProductCodeValues where
    toText (ProductCodeValues' ci) = original ci

-- | Represents an enum of /known/ $ProductCodeValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProductCodeValues where
    toEnum i = case i of
        0 -> Devpay
        1 -> Marketplace
        _ -> (error . showText) $ "Unknown index for ProductCodeValues: " <> toText i
    fromEnum x = case x of
        Devpay -> 0
        Marketplace -> 1
        ProductCodeValues' name -> (error . showText) $ "Unknown ProductCodeValues: " <> original name

-- | Represents the bounds of /known/ $ProductCodeValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProductCodeValues where
    minBound = Devpay
    maxBound = Marketplace

instance Hashable     ProductCodeValues
instance NFData       ProductCodeValues
instance ToByteString ProductCodeValues
instance ToQuery      ProductCodeValues
instance ToHeader     ProductCodeValues

instance FromXML ProductCodeValues where
    parseXML = parseXMLText "ProductCodeValues"
