{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestStatusEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.PullRequestStatusEnum (
  PullRequestStatusEnum (
    ..
    , Closed
    , Open
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PullRequestStatusEnum = PullRequestStatusEnum' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Closed :: PullRequestStatusEnum
pattern Closed = PullRequestStatusEnum' "CLOSED"

pattern Open :: PullRequestStatusEnum
pattern Open = PullRequestStatusEnum' "OPEN"

{-# COMPLETE
  Closed,
  Open,
  PullRequestStatusEnum' #-}

instance FromText PullRequestStatusEnum where
    parser = (PullRequestStatusEnum' . mk) <$> takeText

instance ToText PullRequestStatusEnum where
    toText (PullRequestStatusEnum' ci) = original ci

-- | Represents an enum of /known/ $PullRequestStatusEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PullRequestStatusEnum where
    toEnum i = case i of
        0 -> Closed
        1 -> Open
        _ -> (error . showText) $ "Unknown index for PullRequestStatusEnum: " <> toText i
    fromEnum x = case x of
        Closed -> 0
        Open -> 1
        PullRequestStatusEnum' name -> (error . showText) $ "Unknown PullRequestStatusEnum: " <> original name

-- | Represents the bounds of /known/ $PullRequestStatusEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PullRequestStatusEnum where
    minBound = Closed
    maxBound = Open

instance Hashable     PullRequestStatusEnum
instance NFData       PullRequestStatusEnum
instance ToByteString PullRequestStatusEnum
instance ToQuery      PullRequestStatusEnum
instance ToHeader     PullRequestStatusEnum

instance ToJSON PullRequestStatusEnum where
    toJSON = toJSONText

instance FromJSON PullRequestStatusEnum where
    parseJSON = parseJSONText "PullRequestStatusEnum"
