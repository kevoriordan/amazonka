{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StepExecutionFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.StepExecutionFilterKey (
  StepExecutionFilterKey (
    ..
    , Action
    , StartTimeAfter
    , StartTimeBefore
    , StepExecutionId
    , StepExecutionStatus
    , StepName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StepExecutionFilterKey = StepExecutionFilterKey' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Action :: StepExecutionFilterKey
pattern Action = StepExecutionFilterKey' "Action"

pattern StartTimeAfter :: StepExecutionFilterKey
pattern StartTimeAfter = StepExecutionFilterKey' "StartTimeAfter"

pattern StartTimeBefore :: StepExecutionFilterKey
pattern StartTimeBefore = StepExecutionFilterKey' "StartTimeBefore"

pattern StepExecutionId :: StepExecutionFilterKey
pattern StepExecutionId = StepExecutionFilterKey' "StepExecutionId"

pattern StepExecutionStatus :: StepExecutionFilterKey
pattern StepExecutionStatus = StepExecutionFilterKey' "StepExecutionStatus"

pattern StepName :: StepExecutionFilterKey
pattern StepName = StepExecutionFilterKey' "StepName"

{-# COMPLETE
  Action,
  StartTimeAfter,
  StartTimeBefore,
  StepExecutionId,
  StepExecutionStatus,
  StepName,
  StepExecutionFilterKey' #-}

instance FromText StepExecutionFilterKey where
    parser = (StepExecutionFilterKey' . mk) <$> takeText

instance ToText StepExecutionFilterKey where
    toText (StepExecutionFilterKey' ci) = original ci

-- | Represents an enum of /known/ $StepExecutionFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StepExecutionFilterKey where
    toEnum i = case i of
        0 -> Action
        1 -> StartTimeAfter
        2 -> StartTimeBefore
        3 -> StepExecutionId
        4 -> StepExecutionStatus
        5 -> StepName
        _ -> (error . showText) $ "Unknown index for StepExecutionFilterKey: " <> toText i
    fromEnum x = case x of
        Action -> 0
        StartTimeAfter -> 1
        StartTimeBefore -> 2
        StepExecutionId -> 3
        StepExecutionStatus -> 4
        StepName -> 5
        StepExecutionFilterKey' name -> (error . showText) $ "Unknown StepExecutionFilterKey: " <> original name

-- | Represents the bounds of /known/ $StepExecutionFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StepExecutionFilterKey where
    minBound = Action
    maxBound = StepName

instance Hashable     StepExecutionFilterKey
instance NFData       StepExecutionFilterKey
instance ToByteString StepExecutionFilterKey
instance ToQuery      StepExecutionFilterKey
instance ToHeader     StepExecutionFilterKey

instance ToJSON StepExecutionFilterKey where
    toJSON = toJSONText
