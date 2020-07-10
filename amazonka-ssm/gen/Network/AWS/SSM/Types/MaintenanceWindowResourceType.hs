{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.MaintenanceWindowResourceType (
  MaintenanceWindowResourceType (
    ..
    , Instance
    , ResourceGroup
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MaintenanceWindowResourceType = MaintenanceWindowResourceType' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern Instance :: MaintenanceWindowResourceType
pattern Instance = MaintenanceWindowResourceType' "INSTANCE"

pattern ResourceGroup :: MaintenanceWindowResourceType
pattern ResourceGroup = MaintenanceWindowResourceType' "RESOURCE_GROUP"

{-# COMPLETE
  Instance,
  ResourceGroup,
  MaintenanceWindowResourceType' #-}

instance FromText MaintenanceWindowResourceType where
    parser = (MaintenanceWindowResourceType' . mk) <$> takeText

instance ToText MaintenanceWindowResourceType where
    toText (MaintenanceWindowResourceType' ci) = original ci

-- | Represents an enum of /known/ $MaintenanceWindowResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MaintenanceWindowResourceType where
    toEnum i = case i of
        0 -> Instance
        1 -> ResourceGroup
        _ -> (error . showText) $ "Unknown index for MaintenanceWindowResourceType: " <> toText i
    fromEnum x = case x of
        Instance -> 0
        ResourceGroup -> 1
        MaintenanceWindowResourceType' name -> (error . showText) $ "Unknown MaintenanceWindowResourceType: " <> original name

-- | Represents the bounds of /known/ $MaintenanceWindowResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MaintenanceWindowResourceType where
    minBound = Instance
    maxBound = ResourceGroup

instance Hashable     MaintenanceWindowResourceType
instance NFData       MaintenanceWindowResourceType
instance ToByteString MaintenanceWindowResourceType
instance ToQuery      MaintenanceWindowResourceType
instance ToHeader     MaintenanceWindowResourceType

instance ToJSON MaintenanceWindowResourceType where
    toJSON = toJSONText

instance FromJSON MaintenanceWindowResourceType where
    parseJSON = parseJSONText "MaintenanceWindowResourceType"
