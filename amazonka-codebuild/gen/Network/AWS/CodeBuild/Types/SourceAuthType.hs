{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceAuthType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.SourceAuthType (
  SourceAuthType (
    ..
    , Oauth
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SourceAuthType = SourceAuthType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Oauth :: SourceAuthType
pattern Oauth = SourceAuthType' "OAUTH"

{-# COMPLETE
  Oauth,
  SourceAuthType' #-}

instance FromText SourceAuthType where
    parser = (SourceAuthType' . mk) <$> takeText

instance ToText SourceAuthType where
    toText (SourceAuthType' ci) = original ci

-- | Represents an enum of /known/ $SourceAuthType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SourceAuthType where
    toEnum i = case i of
        0 -> Oauth
        _ -> (error . showText) $ "Unknown index for SourceAuthType: " <> toText i
    fromEnum x = case x of
        Oauth -> 0
        SourceAuthType' name -> (error . showText) $ "Unknown SourceAuthType: " <> original name

-- | Represents the bounds of /known/ $SourceAuthType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SourceAuthType where
    minBound = Oauth
    maxBound = Oauth

instance Hashable     SourceAuthType
instance NFData       SourceAuthType
instance ToByteString SourceAuthType
instance ToQuery      SourceAuthType
instance ToHeader     SourceAuthType

instance ToJSON SourceAuthType where
    toJSON = toJSONText

instance FromJSON SourceAuthType where
    parseJSON = parseJSONText "SourceAuthType"
