{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryQueryOperatorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryQueryOperatorType (
  InventoryQueryOperatorType (
    ..
    , IQOTBeginWith
    , IQOTEqual
    , IQOTExists
    , IQOTGreaterThan
    , IQOTLessThan
    , IQOTNotEqual
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InventoryQueryOperatorType = InventoryQueryOperatorType' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern IQOTBeginWith :: InventoryQueryOperatorType
pattern IQOTBeginWith = InventoryQueryOperatorType' "BeginWith"

pattern IQOTEqual :: InventoryQueryOperatorType
pattern IQOTEqual = InventoryQueryOperatorType' "Equal"

pattern IQOTExists :: InventoryQueryOperatorType
pattern IQOTExists = InventoryQueryOperatorType' "Exists"

pattern IQOTGreaterThan :: InventoryQueryOperatorType
pattern IQOTGreaterThan = InventoryQueryOperatorType' "GreaterThan"

pattern IQOTLessThan :: InventoryQueryOperatorType
pattern IQOTLessThan = InventoryQueryOperatorType' "LessThan"

pattern IQOTNotEqual :: InventoryQueryOperatorType
pattern IQOTNotEqual = InventoryQueryOperatorType' "NotEqual"

{-# COMPLETE
  IQOTBeginWith,
  IQOTEqual,
  IQOTExists,
  IQOTGreaterThan,
  IQOTLessThan,
  IQOTNotEqual,
  InventoryQueryOperatorType' #-}

instance FromText InventoryQueryOperatorType where
    parser = (InventoryQueryOperatorType' . mk) <$> takeText

instance ToText InventoryQueryOperatorType where
    toText (InventoryQueryOperatorType' ci) = original ci

-- | Represents an enum of /known/ $InventoryQueryOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InventoryQueryOperatorType where
    toEnum i = case i of
        0 -> IQOTBeginWith
        1 -> IQOTEqual
        2 -> IQOTExists
        3 -> IQOTGreaterThan
        4 -> IQOTLessThan
        5 -> IQOTNotEqual
        _ -> (error . showText) $ "Unknown index for InventoryQueryOperatorType: " <> toText i
    fromEnum x = case x of
        IQOTBeginWith -> 0
        IQOTEqual -> 1
        IQOTExists -> 2
        IQOTGreaterThan -> 3
        IQOTLessThan -> 4
        IQOTNotEqual -> 5
        InventoryQueryOperatorType' name -> (error . showText) $ "Unknown InventoryQueryOperatorType: " <> original name

-- | Represents the bounds of /known/ $InventoryQueryOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InventoryQueryOperatorType where
    minBound = IQOTBeginWith
    maxBound = IQOTNotEqual

instance Hashable     InventoryQueryOperatorType
instance NFData       InventoryQueryOperatorType
instance ToByteString InventoryQueryOperatorType
instance ToQuery      InventoryQueryOperatorType
instance ToHeader     InventoryQueryOperatorType

instance ToJSON InventoryQueryOperatorType where
    toJSON = toJSONText
