{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Permission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Permission (
  Permission (
    ..
    , PFullControl
    , PRead
    , PReadAcp
    , PWrite
    , PWriteAcp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data Permission = Permission' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern PFullControl :: Permission
pattern PFullControl = Permission' "FULL_CONTROL"

pattern PRead :: Permission
pattern PRead = Permission' "READ"

pattern PReadAcp :: Permission
pattern PReadAcp = Permission' "READ_ACP"

pattern PWrite :: Permission
pattern PWrite = Permission' "WRITE"

pattern PWriteAcp :: Permission
pattern PWriteAcp = Permission' "WRITE_ACP"

{-# COMPLETE
  PFullControl,
  PRead,
  PReadAcp,
  PWrite,
  PWriteAcp,
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
        0 -> PFullControl
        1 -> PRead
        2 -> PReadAcp
        3 -> PWrite
        4 -> PWriteAcp
        _ -> (error . showText) $ "Unknown index for Permission: " <> toText i
    fromEnum x = case x of
        PFullControl -> 0
        PRead -> 1
        PReadAcp -> 2
        PWrite -> 3
        PWriteAcp -> 4
        Permission' name -> (error . showText) $ "Unknown Permission: " <> original name

-- | Represents the bounds of /known/ $Permission.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Permission where
    minBound = PFullControl
    maxBound = PWriteAcp

instance Hashable     Permission
instance NFData       Permission
instance ToByteString Permission
instance ToQuery      Permission
instance ToHeader     Permission

instance FromXML Permission where
    parseXML = parseXMLText "Permission"

instance ToXML Permission where
    toXML = toXMLText
