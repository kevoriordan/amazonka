{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.FlowDefinitionStatus (
  FlowDefinitionStatus (
    ..
    , FDSActive
    , FDSDeleting
    , FDSFailed
    , FDSInitializing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FlowDefinitionStatus = FlowDefinitionStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern FDSActive :: FlowDefinitionStatus
pattern FDSActive = FlowDefinitionStatus' "Active"

pattern FDSDeleting :: FlowDefinitionStatus
pattern FDSDeleting = FlowDefinitionStatus' "Deleting"

pattern FDSFailed :: FlowDefinitionStatus
pattern FDSFailed = FlowDefinitionStatus' "Failed"

pattern FDSInitializing :: FlowDefinitionStatus
pattern FDSInitializing = FlowDefinitionStatus' "Initializing"

{-# COMPLETE
  FDSActive,
  FDSDeleting,
  FDSFailed,
  FDSInitializing,
  FlowDefinitionStatus' #-}

instance FromText FlowDefinitionStatus where
    parser = (FlowDefinitionStatus' . mk) <$> takeText

instance ToText FlowDefinitionStatus where
    toText (FlowDefinitionStatus' ci) = original ci

-- | Represents an enum of /known/ $FlowDefinitionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FlowDefinitionStatus where
    toEnum i = case i of
        0 -> FDSActive
        1 -> FDSDeleting
        2 -> FDSFailed
        3 -> FDSInitializing
        _ -> (error . showText) $ "Unknown index for FlowDefinitionStatus: " <> toText i
    fromEnum x = case x of
        FDSActive -> 0
        FDSDeleting -> 1
        FDSFailed -> 2
        FDSInitializing -> 3
        FlowDefinitionStatus' name -> (error . showText) $ "Unknown FlowDefinitionStatus: " <> original name

-- | Represents the bounds of /known/ $FlowDefinitionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FlowDefinitionStatus where
    minBound = FDSActive
    maxBound = FDSInitializing

instance Hashable     FlowDefinitionStatus
instance NFData       FlowDefinitionStatus
instance ToByteString FlowDefinitionStatus
instance ToQuery      FlowDefinitionStatus
instance ToHeader     FlowDefinitionStatus

instance FromJSON FlowDefinitionStatus where
    parseJSON = parseJSONText "FlowDefinitionStatus"
