{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LanguageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.LanguageType (
  LanguageType (
    ..
    , Android
    , Base
    , Docker
    , Dotnet
    , Golang
    , Java
    , NodeJs
    , PHP
    , Python
    , Ruby
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LanguageType = LanguageType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Android :: LanguageType
pattern Android = LanguageType' "ANDROID"

pattern Base :: LanguageType
pattern Base = LanguageType' "BASE"

pattern Docker :: LanguageType
pattern Docker = LanguageType' "DOCKER"

pattern Dotnet :: LanguageType
pattern Dotnet = LanguageType' "DOTNET"

pattern Golang :: LanguageType
pattern Golang = LanguageType' "GOLANG"

pattern Java :: LanguageType
pattern Java = LanguageType' "JAVA"

pattern NodeJs :: LanguageType
pattern NodeJs = LanguageType' "NODE_JS"

pattern PHP :: LanguageType
pattern PHP = LanguageType' "PHP"

pattern Python :: LanguageType
pattern Python = LanguageType' "PYTHON"

pattern Ruby :: LanguageType
pattern Ruby = LanguageType' "RUBY"

{-# COMPLETE
  Android,
  Base,
  Docker,
  Dotnet,
  Golang,
  Java,
  NodeJs,
  PHP,
  Python,
  Ruby,
  LanguageType' #-}

instance FromText LanguageType where
    parser = (LanguageType' . mk) <$> takeText

instance ToText LanguageType where
    toText (LanguageType' ci) = original ci

-- | Represents an enum of /known/ $LanguageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LanguageType where
    toEnum i = case i of
        0 -> Android
        1 -> Base
        2 -> Docker
        3 -> Dotnet
        4 -> Golang
        5 -> Java
        6 -> NodeJs
        7 -> PHP
        8 -> Python
        9 -> Ruby
        _ -> (error . showText) $ "Unknown index for LanguageType: " <> toText i
    fromEnum x = case x of
        Android -> 0
        Base -> 1
        Docker -> 2
        Dotnet -> 3
        Golang -> 4
        Java -> 5
        NodeJs -> 6
        PHP -> 7
        Python -> 8
        Ruby -> 9
        LanguageType' name -> (error . showText) $ "Unknown LanguageType: " <> original name

-- | Represents the bounds of /known/ $LanguageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LanguageType where
    minBound = Android
    maxBound = Ruby

instance Hashable     LanguageType
instance NFData       LanguageType
instance ToByteString LanguageType
instance ToQuery      LanguageType
instance ToHeader     LanguageType

instance FromJSON LanguageType where
    parseJSON = parseJSONText "LanguageType"
