{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.MemberPermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Cloud9.Types.MemberPermissions (
  MemberPermissions (
    ..
    , MPReadOnly
    , MPReadWrite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MemberPermissions = MemberPermissions' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern MPReadOnly :: MemberPermissions
pattern MPReadOnly = MemberPermissions' "read-only"

pattern MPReadWrite :: MemberPermissions
pattern MPReadWrite = MemberPermissions' "read-write"

{-# COMPLETE
  MPReadOnly,
  MPReadWrite,
  MemberPermissions' #-}

instance FromText MemberPermissions where
    parser = (MemberPermissions' . mk) <$> takeText

instance ToText MemberPermissions where
    toText (MemberPermissions' ci) = original ci

-- | Represents an enum of /known/ $MemberPermissions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MemberPermissions where
    toEnum i = case i of
        0 -> MPReadOnly
        1 -> MPReadWrite
        _ -> (error . showText) $ "Unknown index for MemberPermissions: " <> toText i
    fromEnum x = case x of
        MPReadOnly -> 0
        MPReadWrite -> 1
        MemberPermissions' name -> (error . showText) $ "Unknown MemberPermissions: " <> original name

-- | Represents the bounds of /known/ $MemberPermissions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MemberPermissions where
    minBound = MPReadOnly
    maxBound = MPReadWrite

instance Hashable     MemberPermissions
instance NFData       MemberPermissions
instance ToByteString MemberPermissions
instance ToQuery      MemberPermissions
instance ToHeader     MemberPermissions

instance ToJSON MemberPermissions where
    toJSON = toJSONText
