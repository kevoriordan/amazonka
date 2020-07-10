{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus (
  ProvisionedProductStatus (
    ..
    , PPSAvailable
    , PPSError'
    , PPSPlanInProgress
    , PPSTainted
    , PPSUnderChange
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProvisionedProductStatus = ProvisionedProductStatus' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern PPSAvailable :: ProvisionedProductStatus
pattern PPSAvailable = ProvisionedProductStatus' "AVAILABLE"

pattern PPSError' :: ProvisionedProductStatus
pattern PPSError' = ProvisionedProductStatus' "ERROR"

pattern PPSPlanInProgress :: ProvisionedProductStatus
pattern PPSPlanInProgress = ProvisionedProductStatus' "PLAN_IN_PROGRESS"

pattern PPSTainted :: ProvisionedProductStatus
pattern PPSTainted = ProvisionedProductStatus' "TAINTED"

pattern PPSUnderChange :: ProvisionedProductStatus
pattern PPSUnderChange = ProvisionedProductStatus' "UNDER_CHANGE"

{-# COMPLETE
  PPSAvailable,
  PPSError',
  PPSPlanInProgress,
  PPSTainted,
  PPSUnderChange,
  ProvisionedProductStatus' #-}

instance FromText ProvisionedProductStatus where
    parser = (ProvisionedProductStatus' . mk) <$> takeText

instance ToText ProvisionedProductStatus where
    toText (ProvisionedProductStatus' ci) = original ci

-- | Represents an enum of /known/ $ProvisionedProductStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProvisionedProductStatus where
    toEnum i = case i of
        0 -> PPSAvailable
        1 -> PPSError'
        2 -> PPSPlanInProgress
        3 -> PPSTainted
        4 -> PPSUnderChange
        _ -> (error . showText) $ "Unknown index for ProvisionedProductStatus: " <> toText i
    fromEnum x = case x of
        PPSAvailable -> 0
        PPSError' -> 1
        PPSPlanInProgress -> 2
        PPSTainted -> 3
        PPSUnderChange -> 4
        ProvisionedProductStatus' name -> (error . showText) $ "Unknown ProvisionedProductStatus: " <> original name

-- | Represents the bounds of /known/ $ProvisionedProductStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProvisionedProductStatus where
    minBound = PPSAvailable
    maxBound = PPSUnderChange

instance Hashable     ProvisionedProductStatus
instance NFData       ProvisionedProductStatus
instance ToByteString ProvisionedProductStatus
instance ToQuery      ProvisionedProductStatus
instance ToHeader     ProvisionedProductStatus

instance FromJSON ProvisionedProductStatus where
    parseJSON = parseJSONText "ProvisionedProductStatus"
