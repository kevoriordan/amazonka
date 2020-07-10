{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.LDAPSType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.LDAPSType (
  LDAPSType (
    ..
    , Client
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LDAPSType = LDAPSType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Client :: LDAPSType
pattern Client = LDAPSType' "Client"

{-# COMPLETE
  Client,
  LDAPSType' #-}

instance FromText LDAPSType where
    parser = (LDAPSType' . mk) <$> takeText

instance ToText LDAPSType where
    toText (LDAPSType' ci) = original ci

-- | Represents an enum of /known/ $LDAPSType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LDAPSType where
    toEnum i = case i of
        0 -> Client
        _ -> (error . showText) $ "Unknown index for LDAPSType: " <> toText i
    fromEnum x = case x of
        Client -> 0
        LDAPSType' name -> (error . showText) $ "Unknown LDAPSType: " <> original name

-- | Represents the bounds of /known/ $LDAPSType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LDAPSType where
    minBound = Client
    maxBound = Client

instance Hashable     LDAPSType
instance NFData       LDAPSType
instance ToByteString LDAPSType
instance ToQuery      LDAPSType
instance ToHeader     LDAPSType

instance ToJSON LDAPSType where
    toJSON = toJSONText
