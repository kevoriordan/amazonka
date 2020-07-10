{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ImportStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.ImportStatus (
  ImportStatus (
    ..
    , ISComplete
    , ISFailed
    , ISInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ImportStatus = ImportStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ISComplete :: ImportStatus
pattern ISComplete = ImportStatus' "COMPLETE"

pattern ISFailed :: ImportStatus
pattern ISFailed = ImportStatus' "FAILED"

pattern ISInProgress :: ImportStatus
pattern ISInProgress = ImportStatus' "IN_PROGRESS"

{-# COMPLETE
  ISComplete,
  ISFailed,
  ISInProgress,
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
        0 -> ISComplete
        1 -> ISFailed
        2 -> ISInProgress
        _ -> (error . showText) $ "Unknown index for ImportStatus: " <> toText i
    fromEnum x = case x of
        ISComplete -> 0
        ISFailed -> 1
        ISInProgress -> 2
        ImportStatus' name -> (error . showText) $ "Unknown ImportStatus: " <> original name

-- | Represents the bounds of /known/ $ImportStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImportStatus where
    minBound = ISComplete
    maxBound = ISInProgress

instance Hashable     ImportStatus
instance NFData       ImportStatus
instance ToByteString ImportStatus
instance ToQuery      ImportStatus
instance ToHeader     ImportStatus

instance FromJSON ImportStatus where
    parseJSON = parseJSONText "ImportStatus"
