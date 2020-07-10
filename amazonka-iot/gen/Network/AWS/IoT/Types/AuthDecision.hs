{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthDecision
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuthDecision (
  AuthDecision (
    ..
    , Allowed
    , ExplicitDeny
    , ImplicitDeny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthDecision = AuthDecision' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Allowed :: AuthDecision
pattern Allowed = AuthDecision' "ALLOWED"

pattern ExplicitDeny :: AuthDecision
pattern ExplicitDeny = AuthDecision' "EXPLICIT_DENY"

pattern ImplicitDeny :: AuthDecision
pattern ImplicitDeny = AuthDecision' "IMPLICIT_DENY"

{-# COMPLETE
  Allowed,
  ExplicitDeny,
  ImplicitDeny,
  AuthDecision' #-}

instance FromText AuthDecision where
    parser = (AuthDecision' . mk) <$> takeText

instance ToText AuthDecision where
    toText (AuthDecision' ci) = original ci

-- | Represents an enum of /known/ $AuthDecision.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthDecision where
    toEnum i = case i of
        0 -> Allowed
        1 -> ExplicitDeny
        2 -> ImplicitDeny
        _ -> (error . showText) $ "Unknown index for AuthDecision: " <> toText i
    fromEnum x = case x of
        Allowed -> 0
        ExplicitDeny -> 1
        ImplicitDeny -> 2
        AuthDecision' name -> (error . showText) $ "Unknown AuthDecision: " <> original name

-- | Represents the bounds of /known/ $AuthDecision.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthDecision where
    minBound = Allowed
    maxBound = ImplicitDeny

instance Hashable     AuthDecision
instance NFData       AuthDecision
instance ToByteString AuthDecision
instance ToQuery      AuthDecision
instance ToHeader     AuthDecision

instance FromJSON AuthDecision where
    parseJSON = parseJSONText "AuthDecision"
