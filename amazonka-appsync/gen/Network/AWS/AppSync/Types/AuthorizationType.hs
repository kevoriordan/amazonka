{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AuthorizationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.AuthorizationType (
  AuthorizationType (
    ..
    , AWSIAM
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthorizationType = AuthorizationType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern AWSIAM :: AuthorizationType
pattern AWSIAM = AuthorizationType' "AWS_IAM"

{-# COMPLETE
  AWSIAM,
  AuthorizationType' #-}

instance FromText AuthorizationType where
    parser = (AuthorizationType' . mk) <$> takeText

instance ToText AuthorizationType where
    toText (AuthorizationType' ci) = original ci

-- | Represents an enum of /known/ $AuthorizationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthorizationType where
    toEnum i = case i of
        0 -> AWSIAM
        _ -> (error . showText) $ "Unknown index for AuthorizationType: " <> toText i
    fromEnum x = case x of
        AWSIAM -> 0
        AuthorizationType' name -> (error . showText) $ "Unknown AuthorizationType: " <> original name

-- | Represents the bounds of /known/ $AuthorizationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthorizationType where
    minBound = AWSIAM
    maxBound = AWSIAM

instance Hashable     AuthorizationType
instance NFData       AuthorizationType
instance ToByteString AuthorizationType
instance ToQuery      AuthorizationType
instance ToHeader     AuthorizationType

instance ToJSON AuthorizationType where
    toJSON = toJSONText

instance FromJSON AuthorizationType where
    parseJSON = parseJSONText "AuthorizationType"
