{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeviceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DeviceType (
  DeviceType (
    ..
    , DTEBS
    , DTInstanceStore
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DeviceType = DeviceType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern DTEBS :: DeviceType
pattern DTEBS = DeviceType' "ebs"

pattern DTInstanceStore :: DeviceType
pattern DTInstanceStore = DeviceType' "instance-store"

{-# COMPLETE
  DTEBS,
  DTInstanceStore,
  DeviceType' #-}

instance FromText DeviceType where
    parser = (DeviceType' . mk) <$> takeText

instance ToText DeviceType where
    toText (DeviceType' ci) = original ci

-- | Represents an enum of /known/ $DeviceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceType where
    toEnum i = case i of
        0 -> DTEBS
        1 -> DTInstanceStore
        _ -> (error . showText) $ "Unknown index for DeviceType: " <> toText i
    fromEnum x = case x of
        DTEBS -> 0
        DTInstanceStore -> 1
        DeviceType' name -> (error . showText) $ "Unknown DeviceType: " <> original name

-- | Represents the bounds of /known/ $DeviceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceType where
    minBound = DTEBS
    maxBound = DTInstanceStore

instance Hashable     DeviceType
instance NFData       DeviceType
instance ToByteString DeviceType
instance ToQuery      DeviceType
instance ToHeader     DeviceType

instance FromXML DeviceType where
    parseXML = parseXMLText "DeviceType"
