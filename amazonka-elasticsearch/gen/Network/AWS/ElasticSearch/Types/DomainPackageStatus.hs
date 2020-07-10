{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainPackageStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.DomainPackageStatus (
  DomainPackageStatus (
    ..
    , DPSActive
    , DPSAssociating
    , DPSAssociationFailed
    , DPSDissociating
    , DPSDissociationFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DomainPackageStatus = DomainPackageStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern DPSActive :: DomainPackageStatus
pattern DPSActive = DomainPackageStatus' "ACTIVE"

pattern DPSAssociating :: DomainPackageStatus
pattern DPSAssociating = DomainPackageStatus' "ASSOCIATING"

pattern DPSAssociationFailed :: DomainPackageStatus
pattern DPSAssociationFailed = DomainPackageStatus' "ASSOCIATION_FAILED"

pattern DPSDissociating :: DomainPackageStatus
pattern DPSDissociating = DomainPackageStatus' "DISSOCIATING"

pattern DPSDissociationFailed :: DomainPackageStatus
pattern DPSDissociationFailed = DomainPackageStatus' "DISSOCIATION_FAILED"

{-# COMPLETE
  DPSActive,
  DPSAssociating,
  DPSAssociationFailed,
  DPSDissociating,
  DPSDissociationFailed,
  DomainPackageStatus' #-}

instance FromText DomainPackageStatus where
    parser = (DomainPackageStatus' . mk) <$> takeText

instance ToText DomainPackageStatus where
    toText (DomainPackageStatus' ci) = original ci

-- | Represents an enum of /known/ $DomainPackageStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DomainPackageStatus where
    toEnum i = case i of
        0 -> DPSActive
        1 -> DPSAssociating
        2 -> DPSAssociationFailed
        3 -> DPSDissociating
        4 -> DPSDissociationFailed
        _ -> (error . showText) $ "Unknown index for DomainPackageStatus: " <> toText i
    fromEnum x = case x of
        DPSActive -> 0
        DPSAssociating -> 1
        DPSAssociationFailed -> 2
        DPSDissociating -> 3
        DPSDissociationFailed -> 4
        DomainPackageStatus' name -> (error . showText) $ "Unknown DomainPackageStatus: " <> original name

-- | Represents the bounds of /known/ $DomainPackageStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DomainPackageStatus where
    minBound = DPSActive
    maxBound = DPSDissociationFailed

instance Hashable     DomainPackageStatus
instance NFData       DomainPackageStatus
instance ToByteString DomainPackageStatus
instance ToQuery      DomainPackageStatus
instance ToHeader     DomainPackageStatus

instance FromJSON DomainPackageStatus where
    parseJSON = parseJSONText "DomainPackageStatus"
