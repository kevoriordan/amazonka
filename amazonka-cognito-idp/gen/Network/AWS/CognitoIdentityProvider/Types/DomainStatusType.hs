{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.DomainStatusType (
  DomainStatusType (
    ..
    , DSTActive
    , DSTCreating
    , DSTDeleting
    , DSTFailed
    , DSTUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DomainStatusType = DomainStatusType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern DSTActive :: DomainStatusType
pattern DSTActive = DomainStatusType' "ACTIVE"

pattern DSTCreating :: DomainStatusType
pattern DSTCreating = DomainStatusType' "CREATING"

pattern DSTDeleting :: DomainStatusType
pattern DSTDeleting = DomainStatusType' "DELETING"

pattern DSTFailed :: DomainStatusType
pattern DSTFailed = DomainStatusType' "FAILED"

pattern DSTUpdating :: DomainStatusType
pattern DSTUpdating = DomainStatusType' "UPDATING"

{-# COMPLETE
  DSTActive,
  DSTCreating,
  DSTDeleting,
  DSTFailed,
  DSTUpdating,
  DomainStatusType' #-}

instance FromText DomainStatusType where
    parser = (DomainStatusType' . mk) <$> takeText

instance ToText DomainStatusType where
    toText (DomainStatusType' ci) = original ci

-- | Represents an enum of /known/ $DomainStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DomainStatusType where
    toEnum i = case i of
        0 -> DSTActive
        1 -> DSTCreating
        2 -> DSTDeleting
        3 -> DSTFailed
        4 -> DSTUpdating
        _ -> (error . showText) $ "Unknown index for DomainStatusType: " <> toText i
    fromEnum x = case x of
        DSTActive -> 0
        DSTCreating -> 1
        DSTDeleting -> 2
        DSTFailed -> 3
        DSTUpdating -> 4
        DomainStatusType' name -> (error . showText) $ "Unknown DomainStatusType: " <> original name

-- | Represents the bounds of /known/ $DomainStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DomainStatusType where
    minBound = DSTActive
    maxBound = DSTUpdating

instance Hashable     DomainStatusType
instance NFData       DomainStatusType
instance ToByteString DomainStatusType
instance ToQuery      DomainStatusType
instance ToHeader     DomainStatusType

instance FromJSON DomainStatusType where
    parseJSON = parseJSONText "DomainStatusType"
