{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ActionType (
  ActionType (
    ..
    , InstanceRefresh
    , PlatformUpdate
    , Unknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActionType = ActionType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern InstanceRefresh :: ActionType
pattern InstanceRefresh = ActionType' "InstanceRefresh"

pattern PlatformUpdate :: ActionType
pattern PlatformUpdate = ActionType' "PlatformUpdate"

pattern Unknown :: ActionType
pattern Unknown = ActionType' "Unknown"

{-# COMPLETE
  InstanceRefresh,
  PlatformUpdate,
  Unknown,
  ActionType' #-}

instance FromText ActionType where
    parser = (ActionType' . mk) <$> takeText

instance ToText ActionType where
    toText (ActionType' ci) = original ci

-- | Represents an enum of /known/ $ActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActionType where
    toEnum i = case i of
        0 -> InstanceRefresh
        1 -> PlatformUpdate
        2 -> Unknown
        _ -> (error . showText) $ "Unknown index for ActionType: " <> toText i
    fromEnum x = case x of
        InstanceRefresh -> 0
        PlatformUpdate -> 1
        Unknown -> 2
        ActionType' name -> (error . showText) $ "Unknown ActionType: " <> original name

-- | Represents the bounds of /known/ $ActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActionType where
    minBound = InstanceRefresh
    maxBound = Unknown

instance Hashable     ActionType
instance NFData       ActionType
instance ToByteString ActionType
instance ToQuery      ActionType
instance ToHeader     ActionType

instance FromXML ActionType where
    parseXML = parseXMLText "ActionType"
