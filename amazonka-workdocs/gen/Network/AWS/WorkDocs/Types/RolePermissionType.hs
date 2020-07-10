{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.RolePermissionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.RolePermissionType (
  RolePermissionType (
    ..
    , Direct
    , Inherited
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RolePermissionType = RolePermissionType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Direct :: RolePermissionType
pattern Direct = RolePermissionType' "DIRECT"

pattern Inherited :: RolePermissionType
pattern Inherited = RolePermissionType' "INHERITED"

{-# COMPLETE
  Direct,
  Inherited,
  RolePermissionType' #-}

instance FromText RolePermissionType where
    parser = (RolePermissionType' . mk) <$> takeText

instance ToText RolePermissionType where
    toText (RolePermissionType' ci) = original ci

-- | Represents an enum of /known/ $RolePermissionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RolePermissionType where
    toEnum i = case i of
        0 -> Direct
        1 -> Inherited
        _ -> (error . showText) $ "Unknown index for RolePermissionType: " <> toText i
    fromEnum x = case x of
        Direct -> 0
        Inherited -> 1
        RolePermissionType' name -> (error . showText) $ "Unknown RolePermissionType: " <> original name

-- | Represents the bounds of /known/ $RolePermissionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RolePermissionType where
    minBound = Direct
    maxBound = Inherited

instance Hashable     RolePermissionType
instance NFData       RolePermissionType
instance ToByteString RolePermissionType
instance ToQuery      RolePermissionType
instance ToHeader     RolePermissionType

instance FromJSON RolePermissionType where
    parseJSON = parseJSONText "RolePermissionType"
