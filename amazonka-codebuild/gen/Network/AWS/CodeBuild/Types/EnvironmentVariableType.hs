{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentVariableType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.EnvironmentVariableType (
  EnvironmentVariableType (
    ..
    , EVTParameterStore
    , EVTPlaintext
    , EVTSecretsManager
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnvironmentVariableType = EnvironmentVariableType' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern EVTParameterStore :: EnvironmentVariableType
pattern EVTParameterStore = EnvironmentVariableType' "PARAMETER_STORE"

pattern EVTPlaintext :: EnvironmentVariableType
pattern EVTPlaintext = EnvironmentVariableType' "PLAINTEXT"

pattern EVTSecretsManager :: EnvironmentVariableType
pattern EVTSecretsManager = EnvironmentVariableType' "SECRETS_MANAGER"

{-# COMPLETE
  EVTParameterStore,
  EVTPlaintext,
  EVTSecretsManager,
  EnvironmentVariableType' #-}

instance FromText EnvironmentVariableType where
    parser = (EnvironmentVariableType' . mk) <$> takeText

instance ToText EnvironmentVariableType where
    toText (EnvironmentVariableType' ci) = original ci

-- | Represents an enum of /known/ $EnvironmentVariableType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EnvironmentVariableType where
    toEnum i = case i of
        0 -> EVTParameterStore
        1 -> EVTPlaintext
        2 -> EVTSecretsManager
        _ -> (error . showText) $ "Unknown index for EnvironmentVariableType: " <> toText i
    fromEnum x = case x of
        EVTParameterStore -> 0
        EVTPlaintext -> 1
        EVTSecretsManager -> 2
        EnvironmentVariableType' name -> (error . showText) $ "Unknown EnvironmentVariableType: " <> original name

-- | Represents the bounds of /known/ $EnvironmentVariableType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnvironmentVariableType where
    minBound = EVTParameterStore
    maxBound = EVTSecretsManager

instance Hashable     EnvironmentVariableType
instance NFData       EnvironmentVariableType
instance ToByteString EnvironmentVariableType
instance ToQuery      EnvironmentVariableType
instance ToHeader     EnvironmentVariableType

instance ToJSON EnvironmentVariableType where
    toJSON = toJSONText

instance FromJSON EnvironmentVariableType where
    parseJSON = parseJSONText "EnvironmentVariableType"
