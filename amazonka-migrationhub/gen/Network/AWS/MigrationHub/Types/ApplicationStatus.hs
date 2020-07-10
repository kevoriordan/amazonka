{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ApplicationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MigrationHub.Types.ApplicationStatus (
  ApplicationStatus (
    ..
    , Completed
    , InProgress
    , NotStarted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ApplicationStatus = ApplicationStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Completed :: ApplicationStatus
pattern Completed = ApplicationStatus' "COMPLETED"

pattern InProgress :: ApplicationStatus
pattern InProgress = ApplicationStatus' "IN_PROGRESS"

pattern NotStarted :: ApplicationStatus
pattern NotStarted = ApplicationStatus' "NOT_STARTED"

{-# COMPLETE
  Completed,
  InProgress,
  NotStarted,
  ApplicationStatus' #-}

instance FromText ApplicationStatus where
    parser = (ApplicationStatus' . mk) <$> takeText

instance ToText ApplicationStatus where
    toText (ApplicationStatus' ci) = original ci

-- | Represents an enum of /known/ $ApplicationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ApplicationStatus where
    toEnum i = case i of
        0 -> Completed
        1 -> InProgress
        2 -> NotStarted
        _ -> (error . showText) $ "Unknown index for ApplicationStatus: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        InProgress -> 1
        NotStarted -> 2
        ApplicationStatus' name -> (error . showText) $ "Unknown ApplicationStatus: " <> original name

-- | Represents the bounds of /known/ $ApplicationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ApplicationStatus where
    minBound = Completed
    maxBound = NotStarted

instance Hashable     ApplicationStatus
instance NFData       ApplicationStatus
instance ToByteString ApplicationStatus
instance ToQuery      ApplicationStatus
instance ToHeader     ApplicationStatus

instance ToJSON ApplicationStatus where
    toJSON = toJSONText

instance FromJSON ApplicationStatus where
    parseJSON = parseJSONText "ApplicationStatus"
