{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.IndexStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.IndexStatus (
  IndexStatus (
    ..
    , ISActive
    , ISCreating
    , ISDeleting
    , ISUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IndexStatus = IndexStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern ISActive :: IndexStatus
pattern ISActive = IndexStatus' "ACTIVE"

pattern ISCreating :: IndexStatus
pattern ISCreating = IndexStatus' "CREATING"

pattern ISDeleting :: IndexStatus
pattern ISDeleting = IndexStatus' "DELETING"

pattern ISUpdating :: IndexStatus
pattern ISUpdating = IndexStatus' "UPDATING"

{-# COMPLETE
  ISActive,
  ISCreating,
  ISDeleting,
  ISUpdating,
  IndexStatus' #-}

instance FromText IndexStatus where
    parser = (IndexStatus' . mk) <$> takeText

instance ToText IndexStatus where
    toText (IndexStatus' ci) = original ci

-- | Represents an enum of /known/ $IndexStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IndexStatus where
    toEnum i = case i of
        0 -> ISActive
        1 -> ISCreating
        2 -> ISDeleting
        3 -> ISUpdating
        _ -> (error . showText) $ "Unknown index for IndexStatus: " <> toText i
    fromEnum x = case x of
        ISActive -> 0
        ISCreating -> 1
        ISDeleting -> 2
        ISUpdating -> 3
        IndexStatus' name -> (error . showText) $ "Unknown IndexStatus: " <> original name

-- | Represents the bounds of /known/ $IndexStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IndexStatus where
    minBound = ISActive
    maxBound = ISUpdating

instance Hashable     IndexStatus
instance NFData       IndexStatus
instance ToByteString IndexStatus
instance ToQuery      IndexStatus
instance ToHeader     IndexStatus

instance FromJSON IndexStatus where
    parseJSON = parseJSONText "IndexStatus"
