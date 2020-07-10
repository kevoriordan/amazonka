{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum (
  ConflictResolutionStrategyTypeEnum (
    ..
    , AcceptDestination
    , AcceptSource
    , Automerge
    , None
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConflictResolutionStrategyTypeEnum = ConflictResolutionStrategyTypeEnum' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern AcceptDestination :: ConflictResolutionStrategyTypeEnum
pattern AcceptDestination = ConflictResolutionStrategyTypeEnum' "ACCEPT_DESTINATION"

pattern AcceptSource :: ConflictResolutionStrategyTypeEnum
pattern AcceptSource = ConflictResolutionStrategyTypeEnum' "ACCEPT_SOURCE"

pattern Automerge :: ConflictResolutionStrategyTypeEnum
pattern Automerge = ConflictResolutionStrategyTypeEnum' "AUTOMERGE"

pattern None :: ConflictResolutionStrategyTypeEnum
pattern None = ConflictResolutionStrategyTypeEnum' "NONE"

{-# COMPLETE
  AcceptDestination,
  AcceptSource,
  Automerge,
  None,
  ConflictResolutionStrategyTypeEnum' #-}

instance FromText ConflictResolutionStrategyTypeEnum where
    parser = (ConflictResolutionStrategyTypeEnum' . mk) <$> takeText

instance ToText ConflictResolutionStrategyTypeEnum where
    toText (ConflictResolutionStrategyTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $ConflictResolutionStrategyTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConflictResolutionStrategyTypeEnum where
    toEnum i = case i of
        0 -> AcceptDestination
        1 -> AcceptSource
        2 -> Automerge
        3 -> None
        _ -> (error . showText) $ "Unknown index for ConflictResolutionStrategyTypeEnum: " <> toText i
    fromEnum x = case x of
        AcceptDestination -> 0
        AcceptSource -> 1
        Automerge -> 2
        None -> 3
        ConflictResolutionStrategyTypeEnum' name -> (error . showText) $ "Unknown ConflictResolutionStrategyTypeEnum: " <> original name

-- | Represents the bounds of /known/ $ConflictResolutionStrategyTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConflictResolutionStrategyTypeEnum where
    minBound = AcceptDestination
    maxBound = None

instance Hashable     ConflictResolutionStrategyTypeEnum
instance NFData       ConflictResolutionStrategyTypeEnum
instance ToByteString ConflictResolutionStrategyTypeEnum
instance ToQuery      ConflictResolutionStrategyTypeEnum
instance ToHeader     ConflictResolutionStrategyTypeEnum

instance ToJSON ConflictResolutionStrategyTypeEnum where
    toJSON = toJSONText
