{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceCollectionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceCollectionType (
  InstanceCollectionType (
    ..
    , InstanceFleet
    , InstanceGroup
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceCollectionType = InstanceCollectionType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern InstanceFleet :: InstanceCollectionType
pattern InstanceFleet = InstanceCollectionType' "INSTANCE_FLEET"

pattern InstanceGroup :: InstanceCollectionType
pattern InstanceGroup = InstanceCollectionType' "INSTANCE_GROUP"

{-# COMPLETE
  InstanceFleet,
  InstanceGroup,
  InstanceCollectionType' #-}

instance FromText InstanceCollectionType where
    parser = (InstanceCollectionType' . mk) <$> takeText

instance ToText InstanceCollectionType where
    toText (InstanceCollectionType' ci) = original ci

-- | Represents an enum of /known/ $InstanceCollectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceCollectionType where
    toEnum i = case i of
        0 -> InstanceFleet
        1 -> InstanceGroup
        _ -> (error . showText) $ "Unknown index for InstanceCollectionType: " <> toText i
    fromEnum x = case x of
        InstanceFleet -> 0
        InstanceGroup -> 1
        InstanceCollectionType' name -> (error . showText) $ "Unknown InstanceCollectionType: " <> original name

-- | Represents the bounds of /known/ $InstanceCollectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceCollectionType where
    minBound = InstanceFleet
    maxBound = InstanceGroup

instance Hashable     InstanceCollectionType
instance NFData       InstanceCollectionType
instance ToByteString InstanceCollectionType
instance ToQuery      InstanceCollectionType
instance ToHeader     InstanceCollectionType

instance FromJSON InstanceCollectionType where
    parseJSON = parseJSONText "InstanceCollectionType"
