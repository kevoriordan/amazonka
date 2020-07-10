{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ShareStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ShareStatus (
  ShareStatus (
    ..
    , Completed
    , CompletedWithErrors
    , Error'
    , InProgress
    , NotStarted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ShareStatus = ShareStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Completed :: ShareStatus
pattern Completed = ShareStatus' "COMPLETED"

pattern CompletedWithErrors :: ShareStatus
pattern CompletedWithErrors = ShareStatus' "COMPLETED_WITH_ERRORS"

pattern Error' :: ShareStatus
pattern Error' = ShareStatus' "ERROR"

pattern InProgress :: ShareStatus
pattern InProgress = ShareStatus' "IN_PROGRESS"

pattern NotStarted :: ShareStatus
pattern NotStarted = ShareStatus' "NOT_STARTED"

{-# COMPLETE
  Completed,
  CompletedWithErrors,
  Error',
  InProgress,
  NotStarted,
  ShareStatus' #-}

instance FromText ShareStatus where
    parser = (ShareStatus' . mk) <$> takeText

instance ToText ShareStatus where
    toText (ShareStatus' ci) = original ci

-- | Represents an enum of /known/ $ShareStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ShareStatus where
    toEnum i = case i of
        0 -> Completed
        1 -> CompletedWithErrors
        2 -> Error'
        3 -> InProgress
        4 -> NotStarted
        _ -> (error . showText) $ "Unknown index for ShareStatus: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        CompletedWithErrors -> 1
        Error' -> 2
        InProgress -> 3
        NotStarted -> 4
        ShareStatus' name -> (error . showText) $ "Unknown ShareStatus: " <> original name

-- | Represents the bounds of /known/ $ShareStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ShareStatus where
    minBound = Completed
    maxBound = NotStarted

instance Hashable     ShareStatus
instance NFData       ShareStatus
instance ToByteString ShareStatus
instance ToQuery      ShareStatus
instance ToHeader     ShareStatus

instance FromJSON ShareStatus where
    parseJSON = parseJSONText "ShareStatus"
