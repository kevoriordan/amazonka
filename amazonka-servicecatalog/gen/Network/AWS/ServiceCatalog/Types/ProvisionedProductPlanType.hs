{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType (
  ProvisionedProductPlanType (
    ..
    , Cloudformation
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProvisionedProductPlanType = ProvisionedProductPlanType' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern Cloudformation :: ProvisionedProductPlanType
pattern Cloudformation = ProvisionedProductPlanType' "CLOUDFORMATION"

{-# COMPLETE
  Cloudformation,
  ProvisionedProductPlanType' #-}

instance FromText ProvisionedProductPlanType where
    parser = (ProvisionedProductPlanType' . mk) <$> takeText

instance ToText ProvisionedProductPlanType where
    toText (ProvisionedProductPlanType' ci) = original ci

-- | Represents an enum of /known/ $ProvisionedProductPlanType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProvisionedProductPlanType where
    toEnum i = case i of
        0 -> Cloudformation
        _ -> (error . showText) $ "Unknown index for ProvisionedProductPlanType: " <> toText i
    fromEnum x = case x of
        Cloudformation -> 0
        ProvisionedProductPlanType' name -> (error . showText) $ "Unknown ProvisionedProductPlanType: " <> original name

-- | Represents the bounds of /known/ $ProvisionedProductPlanType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProvisionedProductPlanType where
    minBound = Cloudformation
    maxBound = Cloudformation

instance Hashable     ProvisionedProductPlanType
instance NFData       ProvisionedProductPlanType
instance ToByteString ProvisionedProductPlanType
instance ToQuery      ProvisionedProductPlanType
instance ToHeader     ProvisionedProductPlanType

instance ToJSON ProvisionedProductPlanType where
    toJSON = toJSONText

instance FromJSON ProvisionedProductPlanType where
    parseJSON = parseJSONText "ProvisionedProductPlanType"
