{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.FailureType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.FailureType (
  FailureType (
    ..
    , CancellationFailed
    , InternalFailure
    , InvalidEnvironmentState
    , PermissionsError
    , RollbackFailed
    , RollbackSuccessful
    , UpdateCancelled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FailureType = FailureType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern CancellationFailed :: FailureType
pattern CancellationFailed = FailureType' "CancellationFailed"

pattern InternalFailure :: FailureType
pattern InternalFailure = FailureType' "InternalFailure"

pattern InvalidEnvironmentState :: FailureType
pattern InvalidEnvironmentState = FailureType' "InvalidEnvironmentState"

pattern PermissionsError :: FailureType
pattern PermissionsError = FailureType' "PermissionsError"

pattern RollbackFailed :: FailureType
pattern RollbackFailed = FailureType' "RollbackFailed"

pattern RollbackSuccessful :: FailureType
pattern RollbackSuccessful = FailureType' "RollbackSuccessful"

pattern UpdateCancelled :: FailureType
pattern UpdateCancelled = FailureType' "UpdateCancelled"

{-# COMPLETE
  CancellationFailed,
  InternalFailure,
  InvalidEnvironmentState,
  PermissionsError,
  RollbackFailed,
  RollbackSuccessful,
  UpdateCancelled,
  FailureType' #-}

instance FromText FailureType where
    parser = (FailureType' . mk) <$> takeText

instance ToText FailureType where
    toText (FailureType' ci) = original ci

-- | Represents an enum of /known/ $FailureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FailureType where
    toEnum i = case i of
        0 -> CancellationFailed
        1 -> InternalFailure
        2 -> InvalidEnvironmentState
        3 -> PermissionsError
        4 -> RollbackFailed
        5 -> RollbackSuccessful
        6 -> UpdateCancelled
        _ -> (error . showText) $ "Unknown index for FailureType: " <> toText i
    fromEnum x = case x of
        CancellationFailed -> 0
        InternalFailure -> 1
        InvalidEnvironmentState -> 2
        PermissionsError -> 3
        RollbackFailed -> 4
        RollbackSuccessful -> 5
        UpdateCancelled -> 6
        FailureType' name -> (error . showText) $ "Unknown FailureType: " <> original name

-- | Represents the bounds of /known/ $FailureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FailureType where
    minBound = CancellationFailed
    maxBound = UpdateCancelled

instance Hashable     FailureType
instance NFData       FailureType
instance ToByteString FailureType
instance ToQuery      FailureType
instance ToHeader     FailureType

instance FromXML FailureType where
    parseXML = parseXMLText "FailureType"
