{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Permission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Permission (
  Permission (
    ..
    , All
    , Alter
    , CreateDatabase
    , CreateTable
    , DataLocationAccess
    , Delete
    , Drop
    , Insert
    , Select
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Permission = Permission' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern All :: Permission
pattern All = Permission' "ALL"

pattern Alter :: Permission
pattern Alter = Permission' "ALTER"

pattern CreateDatabase :: Permission
pattern CreateDatabase = Permission' "CREATE_DATABASE"

pattern CreateTable :: Permission
pattern CreateTable = Permission' "CREATE_TABLE"

pattern DataLocationAccess :: Permission
pattern DataLocationAccess = Permission' "DATA_LOCATION_ACCESS"

pattern Delete :: Permission
pattern Delete = Permission' "DELETE"

pattern Drop :: Permission
pattern Drop = Permission' "DROP"

pattern Insert :: Permission
pattern Insert = Permission' "INSERT"

pattern Select :: Permission
pattern Select = Permission' "SELECT"

{-# COMPLETE
  All,
  Alter,
  CreateDatabase,
  CreateTable,
  DataLocationAccess,
  Delete,
  Drop,
  Insert,
  Select,
  Permission' #-}

instance FromText Permission where
    parser = (Permission' . mk) <$> takeText

instance ToText Permission where
    toText (Permission' ci) = original ci

-- | Represents an enum of /known/ $Permission.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Permission where
    toEnum i = case i of
        0 -> All
        1 -> Alter
        2 -> CreateDatabase
        3 -> CreateTable
        4 -> DataLocationAccess
        5 -> Delete
        6 -> Drop
        7 -> Insert
        8 -> Select
        _ -> (error . showText) $ "Unknown index for Permission: " <> toText i
    fromEnum x = case x of
        All -> 0
        Alter -> 1
        CreateDatabase -> 2
        CreateTable -> 3
        DataLocationAccess -> 4
        Delete -> 5
        Drop -> 6
        Insert -> 7
        Select -> 8
        Permission' name -> (error . showText) $ "Unknown Permission: " <> original name

-- | Represents the bounds of /known/ $Permission.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Permission where
    minBound = All
    maxBound = Select

instance Hashable     Permission
instance NFData       Permission
instance ToByteString Permission
instance ToQuery      Permission
instance ToHeader     Permission

instance ToJSON Permission where
    toJSON = toJSONText

instance FromJSON Permission where
    parseJSON = parseJSONText "Permission"
