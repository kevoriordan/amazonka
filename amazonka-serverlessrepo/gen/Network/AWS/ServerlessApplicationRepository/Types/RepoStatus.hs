{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.RepoStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServerlessApplicationRepository.Types.RepoStatus (
  RepoStatus (
    ..
    , Active
    , Expired
    , Preparing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RepoStatus = RepoStatus' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Active :: RepoStatus
pattern Active = RepoStatus' "ACTIVE"

pattern Expired :: RepoStatus
pattern Expired = RepoStatus' "EXPIRED"

pattern Preparing :: RepoStatus
pattern Preparing = RepoStatus' "PREPARING"

{-# COMPLETE
  Active,
  Expired,
  Preparing,
  RepoStatus' #-}

instance FromText RepoStatus where
    parser = (RepoStatus' . mk) <$> takeText

instance ToText RepoStatus where
    toText (RepoStatus' ci) = original ci

-- | Represents an enum of /known/ $RepoStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RepoStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Expired
        2 -> Preparing
        _ -> (error . showText) $ "Unknown index for RepoStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Expired -> 1
        Preparing -> 2
        RepoStatus' name -> (error . showText) $ "Unknown RepoStatus: " <> original name

-- | Represents the bounds of /known/ $RepoStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RepoStatus where
    minBound = Active
    maxBound = Preparing

instance Hashable     RepoStatus
instance NFData       RepoStatus
instance ToByteString RepoStatus
instance ToQuery      RepoStatus
instance ToHeader     RepoStatus

instance FromJSON RepoStatus where
    parseJSON = parseJSONText "RepoStatus"
