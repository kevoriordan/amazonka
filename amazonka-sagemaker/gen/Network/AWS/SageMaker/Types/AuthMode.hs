{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AuthMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AuthMode (
  AuthMode (
    ..
    , IAM
    , SSO
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthMode = AuthMode' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern IAM :: AuthMode
pattern IAM = AuthMode' "IAM"

pattern SSO :: AuthMode
pattern SSO = AuthMode' "SSO"

{-# COMPLETE
  IAM,
  SSO,
  AuthMode' #-}

instance FromText AuthMode where
    parser = (AuthMode' . mk) <$> takeText

instance ToText AuthMode where
    toText (AuthMode' ci) = original ci

-- | Represents an enum of /known/ $AuthMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthMode where
    toEnum i = case i of
        0 -> IAM
        1 -> SSO
        _ -> (error . showText) $ "Unknown index for AuthMode: " <> toText i
    fromEnum x = case x of
        IAM -> 0
        SSO -> 1
        AuthMode' name -> (error . showText) $ "Unknown AuthMode: " <> original name

-- | Represents the bounds of /known/ $AuthMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthMode where
    minBound = IAM
    maxBound = SSO

instance Hashable     AuthMode
instance NFData       AuthMode
instance ToByteString AuthMode
instance ToQuery      AuthMode
instance ToHeader     AuthMode

instance ToJSON AuthMode where
    toJSON = toJSONText

instance FromJSON AuthMode where
    parseJSON = parseJSONText "AuthMode"
