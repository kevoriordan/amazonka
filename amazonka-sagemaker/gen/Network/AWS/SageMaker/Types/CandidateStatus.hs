{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CandidateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CandidateStatus (
  CandidateStatus (
    ..
    , CSCompleted
    , CSFailed
    , CSInProgress
    , CSStopped
    , CSStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CandidateStatus = CandidateStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CSCompleted :: CandidateStatus
pattern CSCompleted = CandidateStatus' "Completed"

pattern CSFailed :: CandidateStatus
pattern CSFailed = CandidateStatus' "Failed"

pattern CSInProgress :: CandidateStatus
pattern CSInProgress = CandidateStatus' "InProgress"

pattern CSStopped :: CandidateStatus
pattern CSStopped = CandidateStatus' "Stopped"

pattern CSStopping :: CandidateStatus
pattern CSStopping = CandidateStatus' "Stopping"

{-# COMPLETE
  CSCompleted,
  CSFailed,
  CSInProgress,
  CSStopped,
  CSStopping,
  CandidateStatus' #-}

instance FromText CandidateStatus where
    parser = (CandidateStatus' . mk) <$> takeText

instance ToText CandidateStatus where
    toText (CandidateStatus' ci) = original ci

-- | Represents an enum of /known/ $CandidateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CandidateStatus where
    toEnum i = case i of
        0 -> CSCompleted
        1 -> CSFailed
        2 -> CSInProgress
        3 -> CSStopped
        4 -> CSStopping
        _ -> (error . showText) $ "Unknown index for CandidateStatus: " <> toText i
    fromEnum x = case x of
        CSCompleted -> 0
        CSFailed -> 1
        CSInProgress -> 2
        CSStopped -> 3
        CSStopping -> 4
        CandidateStatus' name -> (error . showText) $ "Unknown CandidateStatus: " <> original name

-- | Represents the bounds of /known/ $CandidateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CandidateStatus where
    minBound = CSCompleted
    maxBound = CSStopping

instance Hashable     CandidateStatus
instance NFData       CandidateStatus
instance ToByteString CandidateStatus
instance ToQuery      CandidateStatus
instance ToHeader     CandidateStatus

instance ToJSON CandidateStatus where
    toJSON = toJSONText

instance FromJSON CandidateStatus where
    parseJSON = parseJSONText "CandidateStatus"
