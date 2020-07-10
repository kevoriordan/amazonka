{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode (
  BusinessReportFailureCode (
    ..
    , AccessDenied
    , InternalFailure
    , NoSuchBucket
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BusinessReportFailureCode = BusinessReportFailureCode' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern AccessDenied :: BusinessReportFailureCode
pattern AccessDenied = BusinessReportFailureCode' "ACCESS_DENIED"

pattern InternalFailure :: BusinessReportFailureCode
pattern InternalFailure = BusinessReportFailureCode' "INTERNAL_FAILURE"

pattern NoSuchBucket :: BusinessReportFailureCode
pattern NoSuchBucket = BusinessReportFailureCode' "NO_SUCH_BUCKET"

{-# COMPLETE
  AccessDenied,
  InternalFailure,
  NoSuchBucket,
  BusinessReportFailureCode' #-}

instance FromText BusinessReportFailureCode where
    parser = (BusinessReportFailureCode' . mk) <$> takeText

instance ToText BusinessReportFailureCode where
    toText (BusinessReportFailureCode' ci) = original ci

-- | Represents an enum of /known/ $BusinessReportFailureCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BusinessReportFailureCode where
    toEnum i = case i of
        0 -> AccessDenied
        1 -> InternalFailure
        2 -> NoSuchBucket
        _ -> (error . showText) $ "Unknown index for BusinessReportFailureCode: " <> toText i
    fromEnum x = case x of
        AccessDenied -> 0
        InternalFailure -> 1
        NoSuchBucket -> 2
        BusinessReportFailureCode' name -> (error . showText) $ "Unknown BusinessReportFailureCode: " <> original name

-- | Represents the bounds of /known/ $BusinessReportFailureCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BusinessReportFailureCode where
    minBound = AccessDenied
    maxBound = NoSuchBucket

instance Hashable     BusinessReportFailureCode
instance NFData       BusinessReportFailureCode
instance ToByteString BusinessReportFailureCode
instance ToQuery      BusinessReportFailureCode
instance ToHeader     BusinessReportFailureCode

instance FromJSON BusinessReportFailureCode where
    parseJSON = parseJSONText "BusinessReportFailureCode"
