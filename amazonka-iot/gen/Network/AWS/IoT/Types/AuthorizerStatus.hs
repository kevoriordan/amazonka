{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuthorizerStatus (
  AuthorizerStatus (
    ..
    , Active
    , Inactive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthorizerStatus = AuthorizerStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Active :: AuthorizerStatus
pattern Active = AuthorizerStatus' "ACTIVE"

pattern Inactive :: AuthorizerStatus
pattern Inactive = AuthorizerStatus' "INACTIVE"

{-# COMPLETE
  Active,
  Inactive,
  AuthorizerStatus' #-}

instance FromText AuthorizerStatus where
    parser = (AuthorizerStatus' . mk) <$> takeText

instance ToText AuthorizerStatus where
    toText (AuthorizerStatus' ci) = original ci

-- | Represents an enum of /known/ $AuthorizerStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthorizerStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Inactive
        _ -> (error . showText) $ "Unknown index for AuthorizerStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Inactive -> 1
        AuthorizerStatus' name -> (error . showText) $ "Unknown AuthorizerStatus: " <> original name

-- | Represents the bounds of /known/ $AuthorizerStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthorizerStatus where
    minBound = Active
    maxBound = Inactive

instance Hashable     AuthorizerStatus
instance NFData       AuthorizerStatus
instance ToByteString AuthorizerStatus
instance ToQuery      AuthorizerStatus
instance ToHeader     AuthorizerStatus

instance ToJSON AuthorizerStatus where
    toJSON = toJSONText

instance FromJSON AuthorizerStatus where
    parseJSON = parseJSONText "AuthorizerStatus"
