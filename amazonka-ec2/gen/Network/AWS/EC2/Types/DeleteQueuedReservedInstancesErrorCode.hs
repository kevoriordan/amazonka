{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode (
  DeleteQueuedReservedInstancesErrorCode (
    ..
    , DQRIECReservedInstancesIdInvalid
    , DQRIECReservedInstancesNotInQueuedState
    , DQRIECUnexpectedError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DeleteQueuedReservedInstancesErrorCode = DeleteQueuedReservedInstancesErrorCode' (CI
                                                                                         Text)
                                                deriving (Eq, Ord, Read, Show,
                                                          Data, Typeable,
                                                          Generic)

pattern DQRIECReservedInstancesIdInvalid :: DeleteQueuedReservedInstancesErrorCode
pattern DQRIECReservedInstancesIdInvalid = DeleteQueuedReservedInstancesErrorCode' "reserved-instances-id-invalid"

pattern DQRIECReservedInstancesNotInQueuedState :: DeleteQueuedReservedInstancesErrorCode
pattern DQRIECReservedInstancesNotInQueuedState = DeleteQueuedReservedInstancesErrorCode' "reserved-instances-not-in-queued-state"

pattern DQRIECUnexpectedError :: DeleteQueuedReservedInstancesErrorCode
pattern DQRIECUnexpectedError = DeleteQueuedReservedInstancesErrorCode' "unexpected-error"

{-# COMPLETE
  DQRIECReservedInstancesIdInvalid,
  DQRIECReservedInstancesNotInQueuedState,
  DQRIECUnexpectedError,
  DeleteQueuedReservedInstancesErrorCode' #-}

instance FromText DeleteQueuedReservedInstancesErrorCode where
    parser = (DeleteQueuedReservedInstancesErrorCode' . mk) <$> takeText

instance ToText DeleteQueuedReservedInstancesErrorCode where
    toText (DeleteQueuedReservedInstancesErrorCode' ci) = original ci

-- | Represents an enum of /known/ $DeleteQueuedReservedInstancesErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeleteQueuedReservedInstancesErrorCode where
    toEnum i = case i of
        0 -> DQRIECReservedInstancesIdInvalid
        1 -> DQRIECReservedInstancesNotInQueuedState
        2 -> DQRIECUnexpectedError
        _ -> (error . showText) $ "Unknown index for DeleteQueuedReservedInstancesErrorCode: " <> toText i
    fromEnum x = case x of
        DQRIECReservedInstancesIdInvalid -> 0
        DQRIECReservedInstancesNotInQueuedState -> 1
        DQRIECUnexpectedError -> 2
        DeleteQueuedReservedInstancesErrorCode' name -> (error . showText) $ "Unknown DeleteQueuedReservedInstancesErrorCode: " <> original name

-- | Represents the bounds of /known/ $DeleteQueuedReservedInstancesErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeleteQueuedReservedInstancesErrorCode where
    minBound = DQRIECReservedInstancesIdInvalid
    maxBound = DQRIECUnexpectedError

instance Hashable     DeleteQueuedReservedInstancesErrorCode
instance NFData       DeleteQueuedReservedInstancesErrorCode
instance ToByteString DeleteQueuedReservedInstancesErrorCode
instance ToQuery      DeleteQueuedReservedInstancesErrorCode
instance ToHeader     DeleteQueuedReservedInstancesErrorCode

instance FromXML DeleteQueuedReservedInstancesErrorCode where
    parseXML = parseXMLText "DeleteQueuedReservedInstancesErrorCode"
