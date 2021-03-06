{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.InstanceAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.InstanceAction (
  InstanceAction (
    ..
    , KeepAlive
    , Terminate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceAction = InstanceAction' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern KeepAlive :: InstanceAction
pattern KeepAlive = InstanceAction' "KEEP_ALIVE"

pattern Terminate :: InstanceAction
pattern Terminate = InstanceAction' "TERMINATE"

{-# COMPLETE
  KeepAlive,
  Terminate,
  InstanceAction' #-}

instance FromText InstanceAction where
    parser = (InstanceAction' . mk) <$> takeText

instance ToText InstanceAction where
    toText (InstanceAction' ci) = original ci

-- | Represents an enum of /known/ $InstanceAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceAction where
    toEnum i = case i of
        0 -> KeepAlive
        1 -> Terminate
        _ -> (error . showText) $ "Unknown index for InstanceAction: " <> toText i
    fromEnum x = case x of
        KeepAlive -> 0
        Terminate -> 1
        InstanceAction' name -> (error . showText) $ "Unknown InstanceAction: " <> original name

-- | Represents the bounds of /known/ $InstanceAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceAction where
    minBound = KeepAlive
    maxBound = Terminate

instance Hashable     InstanceAction
instance NFData       InstanceAction
instance ToByteString InstanceAction
instance ToQuery      InstanceAction
instance ToHeader     InstanceAction

instance ToJSON InstanceAction where
    toJSON = toJSONText

instance FromJSON InstanceAction where
    parseJSON = parseJSONText "InstanceAction"
