{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RuleAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.RuleAction (
  RuleAction (
    ..
    , Allow
    , Deny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data RuleAction = RuleAction' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Allow :: RuleAction
pattern Allow = RuleAction' "allow"

pattern Deny :: RuleAction
pattern Deny = RuleAction' "deny"

{-# COMPLETE
  Allow,
  Deny,
  RuleAction' #-}

instance FromText RuleAction where
    parser = (RuleAction' . mk) <$> takeText

instance ToText RuleAction where
    toText (RuleAction' ci) = original ci

-- | Represents an enum of /known/ $RuleAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RuleAction where
    toEnum i = case i of
        0 -> Allow
        1 -> Deny
        _ -> (error . showText) $ "Unknown index for RuleAction: " <> toText i
    fromEnum x = case x of
        Allow -> 0
        Deny -> 1
        RuleAction' name -> (error . showText) $ "Unknown RuleAction: " <> original name

-- | Represents the bounds of /known/ $RuleAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RuleAction where
    minBound = Allow
    maxBound = Deny

instance Hashable     RuleAction
instance NFData       RuleAction
instance ToByteString RuleAction
instance ToQuery      RuleAction
instance ToHeader     RuleAction

instance FromXML RuleAction where
    parseXML = parseXMLText "RuleAction"
