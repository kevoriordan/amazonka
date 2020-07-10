{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.ImportStatus (
  ImportStatus (
    ..
    , DeleteComplete
    , DeleteFailed
    , DeleteFailedLimitExceeded
    , DeleteInProgress
    , ImportComplete
    , ImportCompleteWithErrors
    , ImportFailed
    , ImportFailedRecordLimitExceeded
    , ImportFailedServerLimitExceeded
    , ImportInProgress
    , InternalError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ImportStatus = ImportStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern DeleteComplete :: ImportStatus
pattern DeleteComplete = ImportStatus' "DELETE_COMPLETE"

pattern DeleteFailed :: ImportStatus
pattern DeleteFailed = ImportStatus' "DELETE_FAILED"

pattern DeleteFailedLimitExceeded :: ImportStatus
pattern DeleteFailedLimitExceeded = ImportStatus' "DELETE_FAILED_LIMIT_EXCEEDED"

pattern DeleteInProgress :: ImportStatus
pattern DeleteInProgress = ImportStatus' "DELETE_IN_PROGRESS"

pattern ImportComplete :: ImportStatus
pattern ImportComplete = ImportStatus' "IMPORT_COMPLETE"

pattern ImportCompleteWithErrors :: ImportStatus
pattern ImportCompleteWithErrors = ImportStatus' "IMPORT_COMPLETE_WITH_ERRORS"

pattern ImportFailed :: ImportStatus
pattern ImportFailed = ImportStatus' "IMPORT_FAILED"

pattern ImportFailedRecordLimitExceeded :: ImportStatus
pattern ImportFailedRecordLimitExceeded = ImportStatus' "IMPORT_FAILED_RECORD_LIMIT_EXCEEDED"

pattern ImportFailedServerLimitExceeded :: ImportStatus
pattern ImportFailedServerLimitExceeded = ImportStatus' "IMPORT_FAILED_SERVER_LIMIT_EXCEEDED"

pattern ImportInProgress :: ImportStatus
pattern ImportInProgress = ImportStatus' "IMPORT_IN_PROGRESS"

pattern InternalError :: ImportStatus
pattern InternalError = ImportStatus' "INTERNAL_ERROR"

{-# COMPLETE
  DeleteComplete,
  DeleteFailed,
  DeleteFailedLimitExceeded,
  DeleteInProgress,
  ImportComplete,
  ImportCompleteWithErrors,
  ImportFailed,
  ImportFailedRecordLimitExceeded,
  ImportFailedServerLimitExceeded,
  ImportInProgress,
  InternalError,
  ImportStatus' #-}

instance FromText ImportStatus where
    parser = (ImportStatus' . mk) <$> takeText

instance ToText ImportStatus where
    toText (ImportStatus' ci) = original ci

-- | Represents an enum of /known/ $ImportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ImportStatus where
    toEnum i = case i of
        0 -> DeleteComplete
        1 -> DeleteFailed
        2 -> DeleteFailedLimitExceeded
        3 -> DeleteInProgress
        4 -> ImportComplete
        5 -> ImportCompleteWithErrors
        6 -> ImportFailed
        7 -> ImportFailedRecordLimitExceeded
        8 -> ImportFailedServerLimitExceeded
        9 -> ImportInProgress
        10 -> InternalError
        _ -> (error . showText) $ "Unknown index for ImportStatus: " <> toText i
    fromEnum x = case x of
        DeleteComplete -> 0
        DeleteFailed -> 1
        DeleteFailedLimitExceeded -> 2
        DeleteInProgress -> 3
        ImportComplete -> 4
        ImportCompleteWithErrors -> 5
        ImportFailed -> 6
        ImportFailedRecordLimitExceeded -> 7
        ImportFailedServerLimitExceeded -> 8
        ImportInProgress -> 9
        InternalError -> 10
        ImportStatus' name -> (error . showText) $ "Unknown ImportStatus: " <> original name

-- | Represents the bounds of /known/ $ImportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImportStatus where
    minBound = DeleteComplete
    maxBound = InternalError

instance Hashable     ImportStatus
instance NFData       ImportStatus
instance ToByteString ImportStatus
instance ToQuery      ImportStatus
instance ToHeader     ImportStatus

instance FromJSON ImportStatus where
    parseJSON = parseJSONText "ImportStatus"
