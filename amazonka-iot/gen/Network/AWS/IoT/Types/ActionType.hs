{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ActionType (
  ActionType (
    ..
    , Connect
    , Publish
    , Receive
    , Subscribe
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActionType = ActionType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Connect :: ActionType
pattern Connect = ActionType' "CONNECT"

pattern Publish :: ActionType
pattern Publish = ActionType' "PUBLISH"

pattern Receive :: ActionType
pattern Receive = ActionType' "RECEIVE"

pattern Subscribe :: ActionType
pattern Subscribe = ActionType' "SUBSCRIBE"

{-# COMPLETE
  Connect,
  Publish,
  Receive,
  Subscribe,
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
        0 -> Connect
        1 -> Publish
        2 -> Receive
        3 -> Subscribe
        _ -> (error . showText) $ "Unknown index for ActionType: " <> toText i
    fromEnum x = case x of
        Connect -> 0
        Publish -> 1
        Receive -> 2
        Subscribe -> 3
        ActionType' name -> (error . showText) $ "Unknown ActionType: " <> original name

-- | Represents the bounds of /known/ $ActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActionType where
    minBound = Connect
    maxBound = Subscribe

instance Hashable     ActionType
instance NFData       ActionType
instance ToByteString ActionType
instance ToQuery      ActionType
instance ToHeader     ActionType

instance ToJSON ActionType where
    toJSON = toJSONText

instance FromJSON ActionType where
    parseJSON = parseJSONText "ActionType"
