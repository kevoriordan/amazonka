{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProviderStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.CapacityProviderStatus (
  CapacityProviderStatus (
    ..
    , CPSActive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CapacityProviderStatus = CapacityProviderStatus' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern CPSActive :: CapacityProviderStatus
pattern CPSActive = CapacityProviderStatus' "ACTIVE"

{-# COMPLETE
  CPSActive,
  CapacityProviderStatus' #-}

instance FromText CapacityProviderStatus where
    parser = (CapacityProviderStatus' . mk) <$> takeText

instance ToText CapacityProviderStatus where
    toText (CapacityProviderStatus' ci) = original ci

-- | Represents an enum of /known/ $CapacityProviderStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CapacityProviderStatus where
    toEnum i = case i of
        0 -> CPSActive
        _ -> (error . showText) $ "Unknown index for CapacityProviderStatus: " <> toText i
    fromEnum x = case x of
        CPSActive -> 0
        CapacityProviderStatus' name -> (error . showText) $ "Unknown CapacityProviderStatus: " <> original name

-- | Represents the bounds of /known/ $CapacityProviderStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CapacityProviderStatus where
    minBound = CPSActive
    maxBound = CPSActive

instance Hashable     CapacityProviderStatus
instance NFData       CapacityProviderStatus
instance ToByteString CapacityProviderStatus
instance ToQuery      CapacityProviderStatus
instance ToHeader     CapacityProviderStatus

instance FromJSON CapacityProviderStatus where
    parseJSON = parseJSONText "CapacityProviderStatus"
