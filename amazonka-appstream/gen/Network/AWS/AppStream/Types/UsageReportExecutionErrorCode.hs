{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UsageReportExecutionErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.UsageReportExecutionErrorCode (
  UsageReportExecutionErrorCode (
    ..
    , UREECAccessDenied
    , UREECInternalServiceError
    , UREECResourceNotFound
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UsageReportExecutionErrorCode = UsageReportExecutionErrorCode' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern UREECAccessDenied :: UsageReportExecutionErrorCode
pattern UREECAccessDenied = UsageReportExecutionErrorCode' "ACCESS_DENIED"

pattern UREECInternalServiceError :: UsageReportExecutionErrorCode
pattern UREECInternalServiceError = UsageReportExecutionErrorCode' "INTERNAL_SERVICE_ERROR"

pattern UREECResourceNotFound :: UsageReportExecutionErrorCode
pattern UREECResourceNotFound = UsageReportExecutionErrorCode' "RESOURCE_NOT_FOUND"

{-# COMPLETE
  UREECAccessDenied,
  UREECInternalServiceError,
  UREECResourceNotFound,
  UsageReportExecutionErrorCode' #-}

instance FromText UsageReportExecutionErrorCode where
    parser = (UsageReportExecutionErrorCode' . mk) <$> takeText

instance ToText UsageReportExecutionErrorCode where
    toText (UsageReportExecutionErrorCode' ci) = original ci

-- | Represents an enum of /known/ $UsageReportExecutionErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UsageReportExecutionErrorCode where
    toEnum i = case i of
        0 -> UREECAccessDenied
        1 -> UREECInternalServiceError
        2 -> UREECResourceNotFound
        _ -> (error . showText) $ "Unknown index for UsageReportExecutionErrorCode: " <> toText i
    fromEnum x = case x of
        UREECAccessDenied -> 0
        UREECInternalServiceError -> 1
        UREECResourceNotFound -> 2
        UsageReportExecutionErrorCode' name -> (error . showText) $ "Unknown UsageReportExecutionErrorCode: " <> original name

-- | Represents the bounds of /known/ $UsageReportExecutionErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UsageReportExecutionErrorCode where
    minBound = UREECAccessDenied
    maxBound = UREECResourceNotFound

instance Hashable     UsageReportExecutionErrorCode
instance NFData       UsageReportExecutionErrorCode
instance ToByteString UsageReportExecutionErrorCode
instance ToQuery      UsageReportExecutionErrorCode
instance ToHeader     UsageReportExecutionErrorCode

instance FromJSON UsageReportExecutionErrorCode where
    parseJSON = parseJSONText "UsageReportExecutionErrorCode"
