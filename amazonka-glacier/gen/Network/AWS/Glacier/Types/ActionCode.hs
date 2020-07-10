{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ActionCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.ActionCode (
  ActionCode (
    ..
    , ArchiveRetrieval
    , InventoryRetrieval
    , Select
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActionCode = ActionCode' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ArchiveRetrieval :: ActionCode
pattern ArchiveRetrieval = ActionCode' "ArchiveRetrieval"

pattern InventoryRetrieval :: ActionCode
pattern InventoryRetrieval = ActionCode' "InventoryRetrieval"

pattern Select :: ActionCode
pattern Select = ActionCode' "Select"

{-# COMPLETE
  ArchiveRetrieval,
  InventoryRetrieval,
  Select,
  ActionCode' #-}

instance FromText ActionCode where
    parser = (ActionCode' . mk) <$> takeText

instance ToText ActionCode where
    toText (ActionCode' ci) = original ci

-- | Represents an enum of /known/ $ActionCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActionCode where
    toEnum i = case i of
        0 -> ArchiveRetrieval
        1 -> InventoryRetrieval
        2 -> Select
        _ -> (error . showText) $ "Unknown index for ActionCode: " <> toText i
    fromEnum x = case x of
        ArchiveRetrieval -> 0
        InventoryRetrieval -> 1
        Select -> 2
        ActionCode' name -> (error . showText) $ "Unknown ActionCode: " <> original name

-- | Represents the bounds of /known/ $ActionCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActionCode where
    minBound = ArchiveRetrieval
    maxBound = Select

instance Hashable     ActionCode
instance NFData       ActionCode
instance ToByteString ActionCode
instance ToQuery      ActionCode
instance ToHeader     ActionCode

instance FromJSON ActionCode where
    parseJSON = parseJSONText "ActionCode"
