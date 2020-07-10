{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.DeviceCgroupPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.DeviceCgroupPermission (
  DeviceCgroupPermission (
    ..
    , Mknod
    , Read
    , Write
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeviceCgroupPermission = DeviceCgroupPermission' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Mknod :: DeviceCgroupPermission
pattern Mknod = DeviceCgroupPermission' "MKNOD"

pattern Read :: DeviceCgroupPermission
pattern Read = DeviceCgroupPermission' "READ"

pattern Write :: DeviceCgroupPermission
pattern Write = DeviceCgroupPermission' "WRITE"

{-# COMPLETE
  Mknod,
  Read,
  Write,
  DeviceCgroupPermission' #-}

instance FromText DeviceCgroupPermission where
    parser = (DeviceCgroupPermission' . mk) <$> takeText

instance ToText DeviceCgroupPermission where
    toText (DeviceCgroupPermission' ci) = original ci

-- | Represents an enum of /known/ $DeviceCgroupPermission.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceCgroupPermission where
    toEnum i = case i of
        0 -> Mknod
        1 -> Read
        2 -> Write
        _ -> (error . showText) $ "Unknown index for DeviceCgroupPermission: " <> toText i
    fromEnum x = case x of
        Mknod -> 0
        Read -> 1
        Write -> 2
        DeviceCgroupPermission' name -> (error . showText) $ "Unknown DeviceCgroupPermission: " <> original name

-- | Represents the bounds of /known/ $DeviceCgroupPermission.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceCgroupPermission where
    minBound = Mknod
    maxBound = Write

instance Hashable     DeviceCgroupPermission
instance NFData       DeviceCgroupPermission
instance ToByteString DeviceCgroupPermission
instance ToQuery      DeviceCgroupPermission
instance ToHeader     DeviceCgroupPermission

instance ToJSON DeviceCgroupPermission where
    toJSON = toJSONText

instance FromJSON DeviceCgroupPermission where
    parseJSON = parseJSONText "DeviceCgroupPermission"
