{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryDeletionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryDeletionStatus (
  InventoryDeletionStatus (
    ..
    , IDSComplete
    , IDSInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InventoryDeletionStatus = InventoryDeletionStatus' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern IDSComplete :: InventoryDeletionStatus
pattern IDSComplete = InventoryDeletionStatus' "Complete"

pattern IDSInProgress :: InventoryDeletionStatus
pattern IDSInProgress = InventoryDeletionStatus' "InProgress"

{-# COMPLETE
  IDSComplete,
  IDSInProgress,
  InventoryDeletionStatus' #-}

instance FromText InventoryDeletionStatus where
    parser = (InventoryDeletionStatus' . mk) <$> takeText

instance ToText InventoryDeletionStatus where
    toText (InventoryDeletionStatus' ci) = original ci

-- | Represents an enum of /known/ $InventoryDeletionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InventoryDeletionStatus where
    toEnum i = case i of
        0 -> IDSComplete
        1 -> IDSInProgress
        _ -> (error . showText) $ "Unknown index for InventoryDeletionStatus: " <> toText i
    fromEnum x = case x of
        IDSComplete -> 0
        IDSInProgress -> 1
        InventoryDeletionStatus' name -> (error . showText) $ "Unknown InventoryDeletionStatus: " <> original name

-- | Represents the bounds of /known/ $InventoryDeletionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InventoryDeletionStatus where
    minBound = IDSComplete
    maxBound = IDSInProgress

instance Hashable     InventoryDeletionStatus
instance NFData       InventoryDeletionStatus
instance ToByteString InventoryDeletionStatus
instance ToQuery      InventoryDeletionStatus
instance ToHeader     InventoryDeletionStatus

instance FromJSON InventoryDeletionStatus where
    parseJSON = parseJSONText "InventoryDeletionStatus"
