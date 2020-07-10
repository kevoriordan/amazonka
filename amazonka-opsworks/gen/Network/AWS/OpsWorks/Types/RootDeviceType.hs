{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.RootDeviceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.RootDeviceType (
  RootDeviceType (
    ..
    , EBS
    , InstanceStore
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RootDeviceType = RootDeviceType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern EBS :: RootDeviceType
pattern EBS = RootDeviceType' "ebs"

pattern InstanceStore :: RootDeviceType
pattern InstanceStore = RootDeviceType' "instance-store"

{-# COMPLETE
  EBS,
  InstanceStore,
  RootDeviceType' #-}

instance FromText RootDeviceType where
    parser = (RootDeviceType' . mk) <$> takeText

instance ToText RootDeviceType where
    toText (RootDeviceType' ci) = original ci

-- | Represents an enum of /known/ $RootDeviceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RootDeviceType where
    toEnum i = case i of
        0 -> EBS
        1 -> InstanceStore
        _ -> (error . showText) $ "Unknown index for RootDeviceType: " <> toText i
    fromEnum x = case x of
        EBS -> 0
        InstanceStore -> 1
        RootDeviceType' name -> (error . showText) $ "Unknown RootDeviceType: " <> original name

-- | Represents the bounds of /known/ $RootDeviceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RootDeviceType where
    minBound = EBS
    maxBound = InstanceStore

instance Hashable     RootDeviceType
instance NFData       RootDeviceType
instance ToByteString RootDeviceType
instance ToQuery      RootDeviceType
instance ToHeader     RootDeviceType

instance ToJSON RootDeviceType where
    toJSON = toJSONText

instance FromJSON RootDeviceType where
    parseJSON = parseJSONText "RootDeviceType"
