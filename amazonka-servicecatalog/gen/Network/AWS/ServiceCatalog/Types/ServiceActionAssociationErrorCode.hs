{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode (
  ServiceActionAssociationErrorCode (
    ..
    , DuplicateResource
    , InternalFailure
    , LimitExceeded
    , ResourceNotFound
    , Throttling
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceActionAssociationErrorCode = ServiceActionAssociationErrorCode' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern DuplicateResource :: ServiceActionAssociationErrorCode
pattern DuplicateResource = ServiceActionAssociationErrorCode' "DUPLICATE_RESOURCE"

pattern InternalFailure :: ServiceActionAssociationErrorCode
pattern InternalFailure = ServiceActionAssociationErrorCode' "INTERNAL_FAILURE"

pattern LimitExceeded :: ServiceActionAssociationErrorCode
pattern LimitExceeded = ServiceActionAssociationErrorCode' "LIMIT_EXCEEDED"

pattern ResourceNotFound :: ServiceActionAssociationErrorCode
pattern ResourceNotFound = ServiceActionAssociationErrorCode' "RESOURCE_NOT_FOUND"

pattern Throttling :: ServiceActionAssociationErrorCode
pattern Throttling = ServiceActionAssociationErrorCode' "THROTTLING"

{-# COMPLETE
  DuplicateResource,
  InternalFailure,
  LimitExceeded,
  ResourceNotFound,
  Throttling,
  ServiceActionAssociationErrorCode' #-}

instance FromText ServiceActionAssociationErrorCode where
    parser = (ServiceActionAssociationErrorCode' . mk) <$> takeText

instance ToText ServiceActionAssociationErrorCode where
    toText (ServiceActionAssociationErrorCode' ci) = original ci

-- | Represents an enum of /known/ $ServiceActionAssociationErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceActionAssociationErrorCode where
    toEnum i = case i of
        0 -> DuplicateResource
        1 -> InternalFailure
        2 -> LimitExceeded
        3 -> ResourceNotFound
        4 -> Throttling
        _ -> (error . showText) $ "Unknown index for ServiceActionAssociationErrorCode: " <> toText i
    fromEnum x = case x of
        DuplicateResource -> 0
        InternalFailure -> 1
        LimitExceeded -> 2
        ResourceNotFound -> 3
        Throttling -> 4
        ServiceActionAssociationErrorCode' name -> (error . showText) $ "Unknown ServiceActionAssociationErrorCode: " <> original name

-- | Represents the bounds of /known/ $ServiceActionAssociationErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceActionAssociationErrorCode where
    minBound = DuplicateResource
    maxBound = Throttling

instance Hashable     ServiceActionAssociationErrorCode
instance NFData       ServiceActionAssociationErrorCode
instance ToByteString ServiceActionAssociationErrorCode
instance ToQuery      ServiceActionAssociationErrorCode
instance ToHeader     ServiceActionAssociationErrorCode

instance FromJSON ServiceActionAssociationErrorCode where
    parseJSON = parseJSONText "ServiceActionAssociationErrorCode"
