{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DefaultAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.DefaultAction (
  DefaultAction (
    ..
    , Allow
    , Deny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DefaultAction = DefaultAction' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Allow :: DefaultAction
pattern Allow = DefaultAction' "ALLOW"

pattern Deny :: DefaultAction
pattern Deny = DefaultAction' "DENY"

{-# COMPLETE
  Allow,
  Deny,
  DefaultAction' #-}

instance FromText DefaultAction where
    parser = (DefaultAction' . mk) <$> takeText

instance ToText DefaultAction where
    toText (DefaultAction' ci) = original ci

-- | Represents an enum of /known/ $DefaultAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DefaultAction where
    toEnum i = case i of
        0 -> Allow
        1 -> Deny
        _ -> (error . showText) $ "Unknown index for DefaultAction: " <> toText i
    fromEnum x = case x of
        Allow -> 0
        Deny -> 1
        DefaultAction' name -> (error . showText) $ "Unknown DefaultAction: " <> original name

-- | Represents the bounds of /known/ $DefaultAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DefaultAction where
    minBound = Allow
    maxBound = Deny

instance Hashable     DefaultAction
instance NFData       DefaultAction
instance ToByteString DefaultAction
instance ToQuery      DefaultAction
instance ToHeader     DefaultAction

instance ToJSON DefaultAction where
    toJSON = toJSONText

instance FromJSON DefaultAction where
    parseJSON = parseJSONText "DefaultAction"
