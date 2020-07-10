{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AddOnType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.AddOnType (
  AddOnType (
    ..
    , AutoSnapshot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AddOnType = AddOnType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern AutoSnapshot :: AddOnType
pattern AutoSnapshot = AddOnType' "AutoSnapshot"

{-# COMPLETE
  AutoSnapshot,
  AddOnType' #-}

instance FromText AddOnType where
    parser = (AddOnType' . mk) <$> takeText

instance ToText AddOnType where
    toText (AddOnType' ci) = original ci

-- | Represents an enum of /known/ $AddOnType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AddOnType where
    toEnum i = case i of
        0 -> AutoSnapshot
        _ -> (error . showText) $ "Unknown index for AddOnType: " <> toText i
    fromEnum x = case x of
        AutoSnapshot -> 0
        AddOnType' name -> (error . showText) $ "Unknown AddOnType: " <> original name

-- | Represents the bounds of /known/ $AddOnType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AddOnType where
    minBound = AutoSnapshot
    maxBound = AutoSnapshot

instance Hashable     AddOnType
instance NFData       AddOnType
instance ToByteString AddOnType
instance ToQuery      AddOnType
instance ToHeader     AddOnType

instance ToJSON AddOnType where
    toJSON = toJSONText
