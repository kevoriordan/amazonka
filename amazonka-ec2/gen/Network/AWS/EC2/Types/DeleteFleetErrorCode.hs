{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DeleteFleetErrorCode (
  DeleteFleetErrorCode (
    ..
    , DFECFleetIdDoesNotExist
    , DFECFleetIdMalformed
    , DFECFleetNotInDeletableState
    , DFECUnexpectedError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DeleteFleetErrorCode = DeleteFleetErrorCode' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern DFECFleetIdDoesNotExist :: DeleteFleetErrorCode
pattern DFECFleetIdDoesNotExist = DeleteFleetErrorCode' "fleetIdDoesNotExist"

pattern DFECFleetIdMalformed :: DeleteFleetErrorCode
pattern DFECFleetIdMalformed = DeleteFleetErrorCode' "fleetIdMalformed"

pattern DFECFleetNotInDeletableState :: DeleteFleetErrorCode
pattern DFECFleetNotInDeletableState = DeleteFleetErrorCode' "fleetNotInDeletableState"

pattern DFECUnexpectedError :: DeleteFleetErrorCode
pattern DFECUnexpectedError = DeleteFleetErrorCode' "unexpectedError"

{-# COMPLETE
  DFECFleetIdDoesNotExist,
  DFECFleetIdMalformed,
  DFECFleetNotInDeletableState,
  DFECUnexpectedError,
  DeleteFleetErrorCode' #-}

instance FromText DeleteFleetErrorCode where
    parser = (DeleteFleetErrorCode' . mk) <$> takeText

instance ToText DeleteFleetErrorCode where
    toText (DeleteFleetErrorCode' ci) = original ci

-- | Represents an enum of /known/ $DeleteFleetErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeleteFleetErrorCode where
    toEnum i = case i of
        0 -> DFECFleetIdDoesNotExist
        1 -> DFECFleetIdMalformed
        2 -> DFECFleetNotInDeletableState
        3 -> DFECUnexpectedError
        _ -> (error . showText) $ "Unknown index for DeleteFleetErrorCode: " <> toText i
    fromEnum x = case x of
        DFECFleetIdDoesNotExist -> 0
        DFECFleetIdMalformed -> 1
        DFECFleetNotInDeletableState -> 2
        DFECUnexpectedError -> 3
        DeleteFleetErrorCode' name -> (error . showText) $ "Unknown DeleteFleetErrorCode: " <> original name

-- | Represents the bounds of /known/ $DeleteFleetErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeleteFleetErrorCode where
    minBound = DFECFleetIdDoesNotExist
    maxBound = DFECUnexpectedError

instance Hashable     DeleteFleetErrorCode
instance NFData       DeleteFleetErrorCode
instance ToByteString DeleteFleetErrorCode
instance ToQuery      DeleteFleetErrorCode
instance ToHeader     DeleteFleetErrorCode

instance FromXML DeleteFleetErrorCode where
    parseXML = parseXMLText "DeleteFleetErrorCode"
