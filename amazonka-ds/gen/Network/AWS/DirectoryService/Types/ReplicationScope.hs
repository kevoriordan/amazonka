{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ReplicationScope
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.ReplicationScope (
  ReplicationScope (
    ..
    , Domain
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReplicationScope = ReplicationScope' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Domain :: ReplicationScope
pattern Domain = ReplicationScope' "Domain"

{-# COMPLETE
  Domain,
  ReplicationScope' #-}

instance FromText ReplicationScope where
    parser = (ReplicationScope' . mk) <$> takeText

instance ToText ReplicationScope where
    toText (ReplicationScope' ci) = original ci

-- | Represents an enum of /known/ $ReplicationScope.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplicationScope where
    toEnum i = case i of
        0 -> Domain
        _ -> (error . showText) $ "Unknown index for ReplicationScope: " <> toText i
    fromEnum x = case x of
        Domain -> 0
        ReplicationScope' name -> (error . showText) $ "Unknown ReplicationScope: " <> original name

-- | Represents the bounds of /known/ $ReplicationScope.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplicationScope where
    minBound = Domain
    maxBound = Domain

instance Hashable     ReplicationScope
instance NFData       ReplicationScope
instance ToByteString ReplicationScope
instance ToQuery      ReplicationScope
instance ToHeader     ReplicationScope

instance FromJSON ReplicationScope where
    parseJSON = parseJSONText "ReplicationScope"
