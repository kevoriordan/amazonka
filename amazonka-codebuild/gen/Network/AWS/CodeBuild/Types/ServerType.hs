{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ServerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ServerType (
  ServerType (
    ..
    , Bitbucket
    , Github
    , GithubEnterprise
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServerType = ServerType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Bitbucket :: ServerType
pattern Bitbucket = ServerType' "BITBUCKET"

pattern Github :: ServerType
pattern Github = ServerType' "GITHUB"

pattern GithubEnterprise :: ServerType
pattern GithubEnterprise = ServerType' "GITHUB_ENTERPRISE"

{-# COMPLETE
  Bitbucket,
  Github,
  GithubEnterprise,
  ServerType' #-}

instance FromText ServerType where
    parser = (ServerType' . mk) <$> takeText

instance ToText ServerType where
    toText (ServerType' ci) = original ci

-- | Represents an enum of /known/ $ServerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServerType where
    toEnum i = case i of
        0 -> Bitbucket
        1 -> Github
        2 -> GithubEnterprise
        _ -> (error . showText) $ "Unknown index for ServerType: " <> toText i
    fromEnum x = case x of
        Bitbucket -> 0
        Github -> 1
        GithubEnterprise -> 2
        ServerType' name -> (error . showText) $ "Unknown ServerType: " <> original name

-- | Represents the bounds of /known/ $ServerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServerType where
    minBound = Bitbucket
    maxBound = GithubEnterprise

instance Hashable     ServerType
instance NFData       ServerType
instance ToByteString ServerType
instance ToQuery      ServerType
instance ToHeader     ServerType

instance ToJSON ServerType where
    toJSON = toJSONText

instance FromJSON ServerType where
    parseJSON = parseJSONText "ServerType"
