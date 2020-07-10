{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStepState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.RemediationExecutionStepState (
  RemediationExecutionStepState (
    ..
    , Failed
    , Pending
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RemediationExecutionStepState = RemediationExecutionStepState' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern Failed :: RemediationExecutionStepState
pattern Failed = RemediationExecutionStepState' "FAILED"

pattern Pending :: RemediationExecutionStepState
pattern Pending = RemediationExecutionStepState' "PENDING"

pattern Succeeded :: RemediationExecutionStepState
pattern Succeeded = RemediationExecutionStepState' "SUCCEEDED"

{-# COMPLETE
  Failed,
  Pending,
  Succeeded,
  RemediationExecutionStepState' #-}

instance FromText RemediationExecutionStepState where
    parser = (RemediationExecutionStepState' . mk) <$> takeText

instance ToText RemediationExecutionStepState where
    toText (RemediationExecutionStepState' ci) = original ci

-- | Represents an enum of /known/ $RemediationExecutionStepState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RemediationExecutionStepState where
    toEnum i = case i of
        0 -> Failed
        1 -> Pending
        2 -> Succeeded
        _ -> (error . showText) $ "Unknown index for RemediationExecutionStepState: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        Pending -> 1
        Succeeded -> 2
        RemediationExecutionStepState' name -> (error . showText) $ "Unknown RemediationExecutionStepState: " <> original name

-- | Represents the bounds of /known/ $RemediationExecutionStepState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RemediationExecutionStepState where
    minBound = Failed
    maxBound = Succeeded

instance Hashable     RemediationExecutionStepState
instance NFData       RemediationExecutionStepState
instance ToByteString RemediationExecutionStepState
instance ToQuery      RemediationExecutionStepState
instance ToHeader     RemediationExecutionStepState

instance FromJSON RemediationExecutionStepState where
    parseJSON = parseJSONText "RemediationExecutionStepState"
