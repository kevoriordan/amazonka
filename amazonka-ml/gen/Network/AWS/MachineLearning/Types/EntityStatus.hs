{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.EntityStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.EntityStatus (
  EntityStatus (
    ..
    , ESCompleted
    , ESDeleted
    , ESFailed
    , ESInprogress
    , ESPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Object status with the following possible values:
--
--
--     * @PENDING@     * @INPROGRESS@     * @FAILED@     * @COMPLETED@     * @DELETED@ 
--
data EntityStatus = EntityStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ESCompleted :: EntityStatus
pattern ESCompleted = EntityStatus' "COMPLETED"

pattern ESDeleted :: EntityStatus
pattern ESDeleted = EntityStatus' "DELETED"

pattern ESFailed :: EntityStatus
pattern ESFailed = EntityStatus' "FAILED"

pattern ESInprogress :: EntityStatus
pattern ESInprogress = EntityStatus' "INPROGRESS"

pattern ESPending :: EntityStatus
pattern ESPending = EntityStatus' "PENDING"

{-# COMPLETE
  ESCompleted,
  ESDeleted,
  ESFailed,
  ESInprogress,
  ESPending,
  EntityStatus' #-}

instance FromText EntityStatus where
    parser = (EntityStatus' . mk) <$> takeText

instance ToText EntityStatus where
    toText (EntityStatus' ci) = original ci

-- | Represents an enum of /known/ $EntityStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EntityStatus where
    toEnum i = case i of
        0 -> ESCompleted
        1 -> ESDeleted
        2 -> ESFailed
        3 -> ESInprogress
        4 -> ESPending
        _ -> (error . showText) $ "Unknown index for EntityStatus: " <> toText i
    fromEnum x = case x of
        ESCompleted -> 0
        ESDeleted -> 1
        ESFailed -> 2
        ESInprogress -> 3
        ESPending -> 4
        EntityStatus' name -> (error . showText) $ "Unknown EntityStatus: " <> original name

-- | Represents the bounds of /known/ $EntityStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EntityStatus where
    minBound = ESCompleted
    maxBound = ESPending

instance Hashable     EntityStatus
instance NFData       EntityStatus
instance ToByteString EntityStatus
instance ToQuery      EntityStatus
instance ToHeader     EntityStatus

instance FromJSON EntityStatus where
    parseJSON = parseJSONText "EntityStatus"
