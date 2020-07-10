{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EntityType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.EntityType (
  EntityType (
    ..
    , ETAWSManagedPolicy
    , ETGroup
    , ETLocalManagedPolicy
    , ETRole
    , ETUser
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EntityType = EntityType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ETAWSManagedPolicy :: EntityType
pattern ETAWSManagedPolicy = EntityType' "AWSManagedPolicy"

pattern ETGroup :: EntityType
pattern ETGroup = EntityType' "Group"

pattern ETLocalManagedPolicy :: EntityType
pattern ETLocalManagedPolicy = EntityType' "LocalManagedPolicy"

pattern ETRole :: EntityType
pattern ETRole = EntityType' "Role"

pattern ETUser :: EntityType
pattern ETUser = EntityType' "User"

{-# COMPLETE
  ETAWSManagedPolicy,
  ETGroup,
  ETLocalManagedPolicy,
  ETRole,
  ETUser,
  EntityType' #-}

instance FromText EntityType where
    parser = (EntityType' . mk) <$> takeText

instance ToText EntityType where
    toText (EntityType' ci) = original ci

-- | Represents an enum of /known/ $EntityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EntityType where
    toEnum i = case i of
        0 -> ETAWSManagedPolicy
        1 -> ETGroup
        2 -> ETLocalManagedPolicy
        3 -> ETRole
        4 -> ETUser
        _ -> (error . showText) $ "Unknown index for EntityType: " <> toText i
    fromEnum x = case x of
        ETAWSManagedPolicy -> 0
        ETGroup -> 1
        ETLocalManagedPolicy -> 2
        ETRole -> 3
        ETUser -> 4
        EntityType' name -> (error . showText) $ "Unknown EntityType: " <> original name

-- | Represents the bounds of /known/ $EntityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EntityType where
    minBound = ETAWSManagedPolicy
    maxBound = ETUser

instance Hashable     EntityType
instance NFData       EntityType
instance ToByteString EntityType
instance ToQuery      EntityType
instance ToHeader     EntityType
