{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookInstanceStatus (
  NotebookInstanceStatus (
    ..
    , NISDeleting
    , NISFailed
    , NISInService
    , NISPending
    , NISStopped
    , NISStopping
    , NISUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotebookInstanceStatus = NotebookInstanceStatus' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern NISDeleting :: NotebookInstanceStatus
pattern NISDeleting = NotebookInstanceStatus' "Deleting"

pattern NISFailed :: NotebookInstanceStatus
pattern NISFailed = NotebookInstanceStatus' "Failed"

pattern NISInService :: NotebookInstanceStatus
pattern NISInService = NotebookInstanceStatus' "InService"

pattern NISPending :: NotebookInstanceStatus
pattern NISPending = NotebookInstanceStatus' "Pending"

pattern NISStopped :: NotebookInstanceStatus
pattern NISStopped = NotebookInstanceStatus' "Stopped"

pattern NISStopping :: NotebookInstanceStatus
pattern NISStopping = NotebookInstanceStatus' "Stopping"

pattern NISUpdating :: NotebookInstanceStatus
pattern NISUpdating = NotebookInstanceStatus' "Updating"

{-# COMPLETE
  NISDeleting,
  NISFailed,
  NISInService,
  NISPending,
  NISStopped,
  NISStopping,
  NISUpdating,
  NotebookInstanceStatus' #-}

instance FromText NotebookInstanceStatus where
    parser = (NotebookInstanceStatus' . mk) <$> takeText

instance ToText NotebookInstanceStatus where
    toText (NotebookInstanceStatus' ci) = original ci

-- | Represents an enum of /known/ $NotebookInstanceStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotebookInstanceStatus where
    toEnum i = case i of
        0 -> NISDeleting
        1 -> NISFailed
        2 -> NISInService
        3 -> NISPending
        4 -> NISStopped
        5 -> NISStopping
        6 -> NISUpdating
        _ -> (error . showText) $ "Unknown index for NotebookInstanceStatus: " <> toText i
    fromEnum x = case x of
        NISDeleting -> 0
        NISFailed -> 1
        NISInService -> 2
        NISPending -> 3
        NISStopped -> 4
        NISStopping -> 5
        NISUpdating -> 6
        NotebookInstanceStatus' name -> (error . showText) $ "Unknown NotebookInstanceStatus: " <> original name

-- | Represents the bounds of /known/ $NotebookInstanceStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotebookInstanceStatus where
    minBound = NISDeleting
    maxBound = NISUpdating

instance Hashable     NotebookInstanceStatus
instance NFData       NotebookInstanceStatus
instance ToByteString NotebookInstanceStatus
instance ToQuery      NotebookInstanceStatus
instance ToHeader     NotebookInstanceStatus

instance ToJSON NotebookInstanceStatus where
    toJSON = toJSONText

instance FromJSON NotebookInstanceStatus where
    parseJSON = parseJSONText "NotebookInstanceStatus"
