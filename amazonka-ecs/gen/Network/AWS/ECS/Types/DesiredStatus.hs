{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DesiredStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.DesiredStatus (
  DesiredStatus (
    ..
    , Pending
    , Running
    , Stopped
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DesiredStatus = DesiredStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Pending :: DesiredStatus
pattern Pending = DesiredStatus' "PENDING"

pattern Running :: DesiredStatus
pattern Running = DesiredStatus' "RUNNING"

pattern Stopped :: DesiredStatus
pattern Stopped = DesiredStatus' "STOPPED"

{-# COMPLETE
  Pending,
  Running,
  Stopped,
  DesiredStatus' #-}

instance FromText DesiredStatus where
    parser = (DesiredStatus' . mk) <$> takeText

instance ToText DesiredStatus where
    toText (DesiredStatus' ci) = original ci

-- | Represents an enum of /known/ $DesiredStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DesiredStatus where
    toEnum i = case i of
        0 -> Pending
        1 -> Running
        2 -> Stopped
        _ -> (error . showText) $ "Unknown index for DesiredStatus: " <> toText i
    fromEnum x = case x of
        Pending -> 0
        Running -> 1
        Stopped -> 2
        DesiredStatus' name -> (error . showText) $ "Unknown DesiredStatus: " <> original name

-- | Represents the bounds of /known/ $DesiredStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DesiredStatus where
    minBound = Pending
    maxBound = Stopped

instance Hashable     DesiredStatus
instance NFData       DesiredStatus
instance ToByteString DesiredStatus
instance ToQuery      DesiredStatus
instance ToHeader     DesiredStatus

instance ToJSON DesiredStatus where
    toJSON = toJSONText
