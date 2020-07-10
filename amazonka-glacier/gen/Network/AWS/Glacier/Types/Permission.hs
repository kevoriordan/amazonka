{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Permission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.Permission (
  Permission (
    ..
    , FullControl
    , Read
    , ReadAcp
    , Write
    , WriteAcp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Permission = Permission' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern FullControl :: Permission
pattern FullControl = Permission' "FULL_CONTROL"

pattern Read :: Permission
pattern Read = Permission' "READ"

pattern ReadAcp :: Permission
pattern ReadAcp = Permission' "READ_ACP"

pattern Write :: Permission
pattern Write = Permission' "WRITE"

pattern WriteAcp :: Permission
pattern WriteAcp = Permission' "WRITE_ACP"

{-# COMPLETE
  FullControl,
  Read,
  ReadAcp,
  Write,
  WriteAcp,
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
        0 -> FullControl
        1 -> Read
        2 -> ReadAcp
        3 -> Write
        4 -> WriteAcp
        _ -> (error . showText) $ "Unknown index for Permission: " <> toText i
    fromEnum x = case x of
        FullControl -> 0
        Read -> 1
        ReadAcp -> 2
        Write -> 3
        WriteAcp -> 4
        Permission' name -> (error . showText) $ "Unknown Permission: " <> original name

-- | Represents the bounds of /known/ $Permission.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Permission where
    minBound = FullControl
    maxBound = WriteAcp

instance Hashable     Permission
instance NFData       Permission
instance ToByteString Permission
instance ToQuery      Permission
instance ToHeader     Permission

instance ToJSON Permission where
    toJSON = toJSONText

instance FromJSON Permission where
    parseJSON = parseJSONText "Permission"
