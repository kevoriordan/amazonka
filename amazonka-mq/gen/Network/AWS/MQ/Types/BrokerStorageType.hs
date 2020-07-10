{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerStorageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.BrokerStorageType (
  BrokerStorageType (
    ..
    , EBS
    , Efs
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The storage type of the broker.
data BrokerStorageType = BrokerStorageType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern EBS :: BrokerStorageType
pattern EBS = BrokerStorageType' "EBS"

pattern Efs :: BrokerStorageType
pattern Efs = BrokerStorageType' "EFS"

{-# COMPLETE
  EBS,
  Efs,
  BrokerStorageType' #-}

instance FromText BrokerStorageType where
    parser = (BrokerStorageType' . mk) <$> takeText

instance ToText BrokerStorageType where
    toText (BrokerStorageType' ci) = original ci

-- | Represents an enum of /known/ $BrokerStorageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BrokerStorageType where
    toEnum i = case i of
        0 -> EBS
        1 -> Efs
        _ -> (error . showText) $ "Unknown index for BrokerStorageType: " <> toText i
    fromEnum x = case x of
        EBS -> 0
        Efs -> 1
        BrokerStorageType' name -> (error . showText) $ "Unknown BrokerStorageType: " <> original name

-- | Represents the bounds of /known/ $BrokerStorageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BrokerStorageType where
    minBound = EBS
    maxBound = Efs

instance Hashable     BrokerStorageType
instance NFData       BrokerStorageType
instance ToByteString BrokerStorageType
instance ToQuery      BrokerStorageType
instance ToHeader     BrokerStorageType

instance ToJSON BrokerStorageType where
    toJSON = toJSONText

instance FromJSON BrokerStorageType where
    parseJSON = parseJSONText "BrokerStorageType"
