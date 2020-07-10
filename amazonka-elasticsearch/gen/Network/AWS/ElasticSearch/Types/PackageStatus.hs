{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.PackageStatus (
  PackageStatus (
    ..
    , Available
    , CopyFailed
    , Copying
    , DeleteFailed
    , Deleted
    , Deleting
    , Validating
    , ValidationFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PackageStatus = PackageStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Available :: PackageStatus
pattern Available = PackageStatus' "AVAILABLE"

pattern CopyFailed :: PackageStatus
pattern CopyFailed = PackageStatus' "COPY_FAILED"

pattern Copying :: PackageStatus
pattern Copying = PackageStatus' "COPYING"

pattern DeleteFailed :: PackageStatus
pattern DeleteFailed = PackageStatus' "DELETE_FAILED"

pattern Deleted :: PackageStatus
pattern Deleted = PackageStatus' "DELETED"

pattern Deleting :: PackageStatus
pattern Deleting = PackageStatus' "DELETING"

pattern Validating :: PackageStatus
pattern Validating = PackageStatus' "VALIDATING"

pattern ValidationFailed :: PackageStatus
pattern ValidationFailed = PackageStatus' "VALIDATION_FAILED"

{-# COMPLETE
  Available,
  CopyFailed,
  Copying,
  DeleteFailed,
  Deleted,
  Deleting,
  Validating,
  ValidationFailed,
  PackageStatus' #-}

instance FromText PackageStatus where
    parser = (PackageStatus' . mk) <$> takeText

instance ToText PackageStatus where
    toText (PackageStatus' ci) = original ci

-- | Represents an enum of /known/ $PackageStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PackageStatus where
    toEnum i = case i of
        0 -> Available
        1 -> CopyFailed
        2 -> Copying
        3 -> DeleteFailed
        4 -> Deleted
        5 -> Deleting
        6 -> Validating
        7 -> ValidationFailed
        _ -> (error . showText) $ "Unknown index for PackageStatus: " <> toText i
    fromEnum x = case x of
        Available -> 0
        CopyFailed -> 1
        Copying -> 2
        DeleteFailed -> 3
        Deleted -> 4
        Deleting -> 5
        Validating -> 6
        ValidationFailed -> 7
        PackageStatus' name -> (error . showText) $ "Unknown PackageStatus: " <> original name

-- | Represents the bounds of /known/ $PackageStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PackageStatus where
    minBound = Available
    maxBound = ValidationFailed

instance Hashable     PackageStatus
instance NFData       PackageStatus
instance ToByteString PackageStatus
instance ToQuery      PackageStatus
instance ToHeader     PackageStatus

instance FromJSON PackageStatus where
    parseJSON = parseJSONText "PackageStatus"
