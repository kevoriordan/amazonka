{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ApprovalState (
  ApprovalState (
    ..
    , ASApprove
    , ASRevoke
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ApprovalState = ApprovalState' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern ASApprove :: ApprovalState
pattern ASApprove = ApprovalState' "APPROVE"

pattern ASRevoke :: ApprovalState
pattern ASRevoke = ApprovalState' "REVOKE"

{-# COMPLETE
  ASApprove,
  ASRevoke,
  ApprovalState' #-}

instance FromText ApprovalState where
    parser = (ApprovalState' . mk) <$> takeText

instance ToText ApprovalState where
    toText (ApprovalState' ci) = original ci

-- | Represents an enum of /known/ $ApprovalState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ApprovalState where
    toEnum i = case i of
        0 -> ASApprove
        1 -> ASRevoke
        _ -> (error . showText) $ "Unknown index for ApprovalState: " <> toText i
    fromEnum x = case x of
        ASApprove -> 0
        ASRevoke -> 1
        ApprovalState' name -> (error . showText) $ "Unknown ApprovalState: " <> original name

-- | Represents the bounds of /known/ $ApprovalState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ApprovalState where
    minBound = ASApprove
    maxBound = ASRevoke

instance Hashable     ApprovalState
instance NFData       ApprovalState
instance ToByteString ApprovalState
instance ToQuery      ApprovalState
instance ToHeader     ApprovalState

instance ToJSON ApprovalState where
    toJSON = toJSONText

instance FromJSON ApprovalState where
    parseJSON = parseJSONText "ApprovalState"
