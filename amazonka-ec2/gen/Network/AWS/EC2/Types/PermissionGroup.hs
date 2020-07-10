{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PermissionGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PermissionGroup (
  PermissionGroup (
    ..
    , All
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PermissionGroup = PermissionGroup' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern All :: PermissionGroup
pattern All = PermissionGroup' "all"

{-# COMPLETE
  All,
  PermissionGroup' #-}

instance FromText PermissionGroup where
    parser = (PermissionGroup' . mk) <$> takeText

instance ToText PermissionGroup where
    toText (PermissionGroup' ci) = original ci

-- | Represents an enum of /known/ $PermissionGroup.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PermissionGroup where
    toEnum i = case i of
        0 -> All
        _ -> (error . showText) $ "Unknown index for PermissionGroup: " <> toText i
    fromEnum x = case x of
        All -> 0
        PermissionGroup' name -> (error . showText) $ "Unknown PermissionGroup: " <> original name

-- | Represents the bounds of /known/ $PermissionGroup.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PermissionGroup where
    minBound = All
    maxBound = All

instance Hashable     PermissionGroup
instance NFData       PermissionGroup
instance ToByteString PermissionGroup
instance ToQuery      PermissionGroup
instance ToHeader     PermissionGroup

instance FromXML PermissionGroup where
    parseXML = parseXMLText "PermissionGroup"
