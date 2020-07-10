{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FailedItemErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.FailedItemErrorCode (
  FailedItemErrorCode (
    ..
    , FIECAccessDenied
    , FIECDuplicateARN
    , FIECInternalError
    , FIECInvalidARN
    , FIECItemDoesNotExist
    , FIECLimitExceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FailedItemErrorCode = FailedItemErrorCode' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern FIECAccessDenied :: FailedItemErrorCode
pattern FIECAccessDenied = FailedItemErrorCode' "ACCESS_DENIED"

pattern FIECDuplicateARN :: FailedItemErrorCode
pattern FIECDuplicateARN = FailedItemErrorCode' "DUPLICATE_ARN"

pattern FIECInternalError :: FailedItemErrorCode
pattern FIECInternalError = FailedItemErrorCode' "INTERNAL_ERROR"

pattern FIECInvalidARN :: FailedItemErrorCode
pattern FIECInvalidARN = FailedItemErrorCode' "INVALID_ARN"

pattern FIECItemDoesNotExist :: FailedItemErrorCode
pattern FIECItemDoesNotExist = FailedItemErrorCode' "ITEM_DOES_NOT_EXIST"

pattern FIECLimitExceeded :: FailedItemErrorCode
pattern FIECLimitExceeded = FailedItemErrorCode' "LIMIT_EXCEEDED"

{-# COMPLETE
  FIECAccessDenied,
  FIECDuplicateARN,
  FIECInternalError,
  FIECInvalidARN,
  FIECItemDoesNotExist,
  FIECLimitExceeded,
  FailedItemErrorCode' #-}

instance FromText FailedItemErrorCode where
    parser = (FailedItemErrorCode' . mk) <$> takeText

instance ToText FailedItemErrorCode where
    toText (FailedItemErrorCode' ci) = original ci

-- | Represents an enum of /known/ $FailedItemErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FailedItemErrorCode where
    toEnum i = case i of
        0 -> FIECAccessDenied
        1 -> FIECDuplicateARN
        2 -> FIECInternalError
        3 -> FIECInvalidARN
        4 -> FIECItemDoesNotExist
        5 -> FIECLimitExceeded
        _ -> (error . showText) $ "Unknown index for FailedItemErrorCode: " <> toText i
    fromEnum x = case x of
        FIECAccessDenied -> 0
        FIECDuplicateARN -> 1
        FIECInternalError -> 2
        FIECInvalidARN -> 3
        FIECItemDoesNotExist -> 4
        FIECLimitExceeded -> 5
        FailedItemErrorCode' name -> (error . showText) $ "Unknown FailedItemErrorCode: " <> original name

-- | Represents the bounds of /known/ $FailedItemErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FailedItemErrorCode where
    minBound = FIECAccessDenied
    maxBound = FIECLimitExceeded

instance Hashable     FailedItemErrorCode
instance NFData       FailedItemErrorCode
instance ToByteString FailedItemErrorCode
instance ToQuery      FailedItemErrorCode
instance ToHeader     FailedItemErrorCode

instance FromJSON FailedItemErrorCode where
    parseJSON = parseJSONText "FailedItemErrorCode"
