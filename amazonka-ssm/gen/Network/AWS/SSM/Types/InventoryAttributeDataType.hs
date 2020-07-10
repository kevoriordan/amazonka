{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryAttributeDataType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryAttributeDataType (
  InventoryAttributeDataType (
    ..
    , IADTNumber
    , IADTString
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InventoryAttributeDataType = InventoryAttributeDataType' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern IADTNumber :: InventoryAttributeDataType
pattern IADTNumber = InventoryAttributeDataType' "number"

pattern IADTString :: InventoryAttributeDataType
pattern IADTString = InventoryAttributeDataType' "string"

{-# COMPLETE
  IADTNumber,
  IADTString,
  InventoryAttributeDataType' #-}

instance FromText InventoryAttributeDataType where
    parser = (InventoryAttributeDataType' . mk) <$> takeText

instance ToText InventoryAttributeDataType where
    toText (InventoryAttributeDataType' ci) = original ci

-- | Represents an enum of /known/ $InventoryAttributeDataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InventoryAttributeDataType where
    toEnum i = case i of
        0 -> IADTNumber
        1 -> IADTString
        _ -> (error . showText) $ "Unknown index for InventoryAttributeDataType: " <> toText i
    fromEnum x = case x of
        IADTNumber -> 0
        IADTString -> 1
        InventoryAttributeDataType' name -> (error . showText) $ "Unknown InventoryAttributeDataType: " <> original name

-- | Represents the bounds of /known/ $InventoryAttributeDataType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InventoryAttributeDataType where
    minBound = IADTNumber
    maxBound = IADTString

instance Hashable     InventoryAttributeDataType
instance NFData       InventoryAttributeDataType
instance ToByteString InventoryAttributeDataType
instance ToQuery      InventoryAttributeDataType
instance ToHeader     InventoryAttributeDataType

instance FromJSON InventoryAttributeDataType where
    parseJSON = parseJSONText "InventoryAttributeDataType"
