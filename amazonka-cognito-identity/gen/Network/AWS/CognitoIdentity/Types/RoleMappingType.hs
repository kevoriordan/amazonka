{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.RoleMappingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.RoleMappingType (
  RoleMappingType (
    ..
    , Rules
    , Token
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RoleMappingType = RoleMappingType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Rules :: RoleMappingType
pattern Rules = RoleMappingType' "Rules"

pattern Token :: RoleMappingType
pattern Token = RoleMappingType' "Token"

{-# COMPLETE
  Rules,
  Token,
  RoleMappingType' #-}

instance FromText RoleMappingType where
    parser = (RoleMappingType' . mk) <$> takeText

instance ToText RoleMappingType where
    toText (RoleMappingType' ci) = original ci

-- | Represents an enum of /known/ $RoleMappingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RoleMappingType where
    toEnum i = case i of
        0 -> Rules
        1 -> Token
        _ -> (error . showText) $ "Unknown index for RoleMappingType: " <> toText i
    fromEnum x = case x of
        Rules -> 0
        Token -> 1
        RoleMappingType' name -> (error . showText) $ "Unknown RoleMappingType: " <> original name

-- | Represents the bounds of /known/ $RoleMappingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RoleMappingType where
    minBound = Rules
    maxBound = Token

instance Hashable     RoleMappingType
instance NFData       RoleMappingType
instance ToByteString RoleMappingType
instance ToQuery      RoleMappingType
instance ToHeader     RoleMappingType

instance ToJSON RoleMappingType where
    toJSON = toJSONText

instance FromJSON RoleMappingType where
    parseJSON = parseJSONText "RoleMappingType"
