{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.FailureType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.FailureType (
  FailureType (
    ..
    , ConfigurationError
    , JobFailed
    , PermissionError
    , RevisionOutOfSync
    , RevisionUnavailable
    , SystemUnavailable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FailureType = FailureType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern ConfigurationError :: FailureType
pattern ConfigurationError = FailureType' "ConfigurationError"

pattern JobFailed :: FailureType
pattern JobFailed = FailureType' "JobFailed"

pattern PermissionError :: FailureType
pattern PermissionError = FailureType' "PermissionError"

pattern RevisionOutOfSync :: FailureType
pattern RevisionOutOfSync = FailureType' "RevisionOutOfSync"

pattern RevisionUnavailable :: FailureType
pattern RevisionUnavailable = FailureType' "RevisionUnavailable"

pattern SystemUnavailable :: FailureType
pattern SystemUnavailable = FailureType' "SystemUnavailable"

{-# COMPLETE
  ConfigurationError,
  JobFailed,
  PermissionError,
  RevisionOutOfSync,
  RevisionUnavailable,
  SystemUnavailable,
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
        0 -> ConfigurationError
        1 -> JobFailed
        2 -> PermissionError
        3 -> RevisionOutOfSync
        4 -> RevisionUnavailable
        5 -> SystemUnavailable
        _ -> (error . showText) $ "Unknown index for FailureType: " <> toText i
    fromEnum x = case x of
        ConfigurationError -> 0
        JobFailed -> 1
        PermissionError -> 2
        RevisionOutOfSync -> 3
        RevisionUnavailable -> 4
        SystemUnavailable -> 5
        FailureType' name -> (error . showText) $ "Unknown FailureType: " <> original name

-- | Represents the bounds of /known/ $FailureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FailureType where
    minBound = ConfigurationError
    maxBound = SystemUnavailable

instance Hashable     FailureType
instance NFData       FailureType
instance ToByteString FailureType
instance ToQuery      FailureType
instance ToHeader     FailureType

instance ToJSON FailureType where
    toJSON = toJSONText
