{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.HandlerErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.HandlerErrorCode (
  HandlerErrorCode (
    ..
    , AccessDenied
    , AlreadyExists
    , GeneralServiceException
    , InternalFailure
    , InvalidCredentials
    , InvalidRequest
    , NetworkFailure
    , NotFound
    , NotStabilized
    , NotUpdatable
    , ResourceConflict
    , ServiceInternalError
    , ServiceLimitExceeded
    , Throttling
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HandlerErrorCode = HandlerErrorCode' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern AccessDenied :: HandlerErrorCode
pattern AccessDenied = HandlerErrorCode' "AccessDenied"

pattern AlreadyExists :: HandlerErrorCode
pattern AlreadyExists = HandlerErrorCode' "AlreadyExists"

pattern GeneralServiceException :: HandlerErrorCode
pattern GeneralServiceException = HandlerErrorCode' "GeneralServiceException"

pattern InternalFailure :: HandlerErrorCode
pattern InternalFailure = HandlerErrorCode' "InternalFailure"

pattern InvalidCredentials :: HandlerErrorCode
pattern InvalidCredentials = HandlerErrorCode' "InvalidCredentials"

pattern InvalidRequest :: HandlerErrorCode
pattern InvalidRequest = HandlerErrorCode' "InvalidRequest"

pattern NetworkFailure :: HandlerErrorCode
pattern NetworkFailure = HandlerErrorCode' "NetworkFailure"

pattern NotFound :: HandlerErrorCode
pattern NotFound = HandlerErrorCode' "NotFound"

pattern NotStabilized :: HandlerErrorCode
pattern NotStabilized = HandlerErrorCode' "NotStabilized"

pattern NotUpdatable :: HandlerErrorCode
pattern NotUpdatable = HandlerErrorCode' "NotUpdatable"

pattern ResourceConflict :: HandlerErrorCode
pattern ResourceConflict = HandlerErrorCode' "ResourceConflict"

pattern ServiceInternalError :: HandlerErrorCode
pattern ServiceInternalError = HandlerErrorCode' "ServiceInternalError"

pattern ServiceLimitExceeded :: HandlerErrorCode
pattern ServiceLimitExceeded = HandlerErrorCode' "ServiceLimitExceeded"

pattern Throttling :: HandlerErrorCode
pattern Throttling = HandlerErrorCode' "Throttling"

{-# COMPLETE
  AccessDenied,
  AlreadyExists,
  GeneralServiceException,
  InternalFailure,
  InvalidCredentials,
  InvalidRequest,
  NetworkFailure,
  NotFound,
  NotStabilized,
  NotUpdatable,
  ResourceConflict,
  ServiceInternalError,
  ServiceLimitExceeded,
  Throttling,
  HandlerErrorCode' #-}

instance FromText HandlerErrorCode where
    parser = (HandlerErrorCode' . mk) <$> takeText

instance ToText HandlerErrorCode where
    toText (HandlerErrorCode' ci) = original ci

-- | Represents an enum of /known/ $HandlerErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HandlerErrorCode where
    toEnum i = case i of
        0 -> AccessDenied
        1 -> AlreadyExists
        2 -> GeneralServiceException
        3 -> InternalFailure
        4 -> InvalidCredentials
        5 -> InvalidRequest
        6 -> NetworkFailure
        7 -> NotFound
        8 -> NotStabilized
        9 -> NotUpdatable
        10 -> ResourceConflict
        11 -> ServiceInternalError
        12 -> ServiceLimitExceeded
        13 -> Throttling
        _ -> (error . showText) $ "Unknown index for HandlerErrorCode: " <> toText i
    fromEnum x = case x of
        AccessDenied -> 0
        AlreadyExists -> 1
        GeneralServiceException -> 2
        InternalFailure -> 3
        InvalidCredentials -> 4
        InvalidRequest -> 5
        NetworkFailure -> 6
        NotFound -> 7
        NotStabilized -> 8
        NotUpdatable -> 9
        ResourceConflict -> 10
        ServiceInternalError -> 11
        ServiceLimitExceeded -> 12
        Throttling -> 13
        HandlerErrorCode' name -> (error . showText) $ "Unknown HandlerErrorCode: " <> original name

-- | Represents the bounds of /known/ $HandlerErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HandlerErrorCode where
    minBound = AccessDenied
    maxBound = Throttling

instance Hashable     HandlerErrorCode
instance NFData       HandlerErrorCode
instance ToByteString HandlerErrorCode
instance ToQuery      HandlerErrorCode
instance ToHeader     HandlerErrorCode
