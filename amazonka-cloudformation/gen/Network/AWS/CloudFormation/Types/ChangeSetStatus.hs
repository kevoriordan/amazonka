{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ChangeSetStatus (
  ChangeSetStatus (
    ..
    , CSSCreateComplete
    , CSSCreateInProgress
    , CSSCreatePending
    , CSSDeleteComplete
    , CSSFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChangeSetStatus = ChangeSetStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CSSCreateComplete :: ChangeSetStatus
pattern CSSCreateComplete = ChangeSetStatus' "CREATE_COMPLETE"

pattern CSSCreateInProgress :: ChangeSetStatus
pattern CSSCreateInProgress = ChangeSetStatus' "CREATE_IN_PROGRESS"

pattern CSSCreatePending :: ChangeSetStatus
pattern CSSCreatePending = ChangeSetStatus' "CREATE_PENDING"

pattern CSSDeleteComplete :: ChangeSetStatus
pattern CSSDeleteComplete = ChangeSetStatus' "DELETE_COMPLETE"

pattern CSSFailed :: ChangeSetStatus
pattern CSSFailed = ChangeSetStatus' "FAILED"

{-# COMPLETE
  CSSCreateComplete,
  CSSCreateInProgress,
  CSSCreatePending,
  CSSDeleteComplete,
  CSSFailed,
  ChangeSetStatus' #-}

instance FromText ChangeSetStatus where
    parser = (ChangeSetStatus' . mk) <$> takeText

instance ToText ChangeSetStatus where
    toText (ChangeSetStatus' ci) = original ci

-- | Represents an enum of /known/ $ChangeSetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeSetStatus where
    toEnum i = case i of
        0 -> CSSCreateComplete
        1 -> CSSCreateInProgress
        2 -> CSSCreatePending
        3 -> CSSDeleteComplete
        4 -> CSSFailed
        _ -> (error . showText) $ "Unknown index for ChangeSetStatus: " <> toText i
    fromEnum x = case x of
        CSSCreateComplete -> 0
        CSSCreateInProgress -> 1
        CSSCreatePending -> 2
        CSSDeleteComplete -> 3
        CSSFailed -> 4
        ChangeSetStatus' name -> (error . showText) $ "Unknown ChangeSetStatus: " <> original name

-- | Represents the bounds of /known/ $ChangeSetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeSetStatus where
    minBound = CSSCreateComplete
    maxBound = CSSFailed

instance Hashable     ChangeSetStatus
instance NFData       ChangeSetStatus
instance ToByteString ChangeSetStatus
instance ToQuery      ChangeSetStatus
instance ToHeader     ChangeSetStatus

instance FromXML ChangeSetStatus where
    parseXML = parseXMLText "ChangeSetStatus"
