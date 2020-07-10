{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReloadOptionValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReloadOptionValue (
  ReloadOptionValue (
    ..
    , DataReload
    , ValidateOnly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReloadOptionValue = ReloadOptionValue' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern DataReload :: ReloadOptionValue
pattern DataReload = ReloadOptionValue' "data-reload"

pattern ValidateOnly :: ReloadOptionValue
pattern ValidateOnly = ReloadOptionValue' "validate-only"

{-# COMPLETE
  DataReload,
  ValidateOnly,
  ReloadOptionValue' #-}

instance FromText ReloadOptionValue where
    parser = (ReloadOptionValue' . mk) <$> takeText

instance ToText ReloadOptionValue where
    toText (ReloadOptionValue' ci) = original ci

-- | Represents an enum of /known/ $ReloadOptionValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReloadOptionValue where
    toEnum i = case i of
        0 -> DataReload
        1 -> ValidateOnly
        _ -> (error . showText) $ "Unknown index for ReloadOptionValue: " <> toText i
    fromEnum x = case x of
        DataReload -> 0
        ValidateOnly -> 1
        ReloadOptionValue' name -> (error . showText) $ "Unknown ReloadOptionValue: " <> original name

-- | Represents the bounds of /known/ $ReloadOptionValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReloadOptionValue where
    minBound = DataReload
    maxBound = ValidateOnly

instance Hashable     ReloadOptionValue
instance NFData       ReloadOptionValue
instance ToByteString ReloadOptionValue
instance ToQuery      ReloadOptionValue
instance ToHeader     ReloadOptionValue

instance ToJSON ReloadOptionValue where
    toJSON = toJSONText
