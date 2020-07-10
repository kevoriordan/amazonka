{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DomainStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.DomainStatus (
  DomainStatus (
    ..
    , Deleting
    , Failed
    , InService
    , Pending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DomainStatus = DomainStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Deleting :: DomainStatus
pattern Deleting = DomainStatus' "Deleting"

pattern Failed :: DomainStatus
pattern Failed = DomainStatus' "Failed"

pattern InService :: DomainStatus
pattern InService = DomainStatus' "InService"

pattern Pending :: DomainStatus
pattern Pending = DomainStatus' "Pending"

{-# COMPLETE
  Deleting,
  Failed,
  InService,
  Pending,
  DomainStatus' #-}

instance FromText DomainStatus where
    parser = (DomainStatus' . mk) <$> takeText

instance ToText DomainStatus where
    toText (DomainStatus' ci) = original ci

-- | Represents an enum of /known/ $DomainStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DomainStatus where
    toEnum i = case i of
        0 -> Deleting
        1 -> Failed
        2 -> InService
        3 -> Pending
        _ -> (error . showText) $ "Unknown index for DomainStatus: " <> toText i
    fromEnum x = case x of
        Deleting -> 0
        Failed -> 1
        InService -> 2
        Pending -> 3
        DomainStatus' name -> (error . showText) $ "Unknown DomainStatus: " <> original name

-- | Represents the bounds of /known/ $DomainStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DomainStatus where
    minBound = Deleting
    maxBound = Pending

instance Hashable     DomainStatus
instance NFData       DomainStatus
instance ToByteString DomainStatus
instance ToQuery      DomainStatus
instance ToHeader     DomainStatus

instance FromJSON DomainStatus where
    parseJSON = parseJSONText "DomainStatus"
