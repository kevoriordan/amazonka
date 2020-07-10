{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CEStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.CEStatus (
  CEStatus (
    ..
    , CESCreating
    , CESDeleted
    , CESDeleting
    , CESInvalid
    , CESUpdating
    , CESValid
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CEStatus = CEStatus' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern CESCreating :: CEStatus
pattern CESCreating = CEStatus' "CREATING"

pattern CESDeleted :: CEStatus
pattern CESDeleted = CEStatus' "DELETED"

pattern CESDeleting :: CEStatus
pattern CESDeleting = CEStatus' "DELETING"

pattern CESInvalid :: CEStatus
pattern CESInvalid = CEStatus' "INVALID"

pattern CESUpdating :: CEStatus
pattern CESUpdating = CEStatus' "UPDATING"

pattern CESValid :: CEStatus
pattern CESValid = CEStatus' "VALID"

{-# COMPLETE
  CESCreating,
  CESDeleted,
  CESDeleting,
  CESInvalid,
  CESUpdating,
  CESValid,
  CEStatus' #-}

instance FromText CEStatus where
    parser = (CEStatus' . mk) <$> takeText

instance ToText CEStatus where
    toText (CEStatus' ci) = original ci

-- | Represents an enum of /known/ $CEStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CEStatus where
    toEnum i = case i of
        0 -> CESCreating
        1 -> CESDeleted
        2 -> CESDeleting
        3 -> CESInvalid
        4 -> CESUpdating
        5 -> CESValid
        _ -> (error . showText) $ "Unknown index for CEStatus: " <> toText i
    fromEnum x = case x of
        CESCreating -> 0
        CESDeleted -> 1
        CESDeleting -> 2
        CESInvalid -> 3
        CESUpdating -> 4
        CESValid -> 5
        CEStatus' name -> (error . showText) $ "Unknown CEStatus: " <> original name

-- | Represents the bounds of /known/ $CEStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CEStatus where
    minBound = CESCreating
    maxBound = CESValid

instance Hashable     CEStatus
instance NFData       CEStatus
instance ToByteString CEStatus
instance ToQuery      CEStatus
instance ToHeader     CEStatus

instance FromJSON CEStatus where
    parseJSON = parseJSONText "CEStatus"
