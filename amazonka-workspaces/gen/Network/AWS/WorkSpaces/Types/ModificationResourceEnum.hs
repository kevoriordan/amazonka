{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ModificationResourceEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.ModificationResourceEnum (
  ModificationResourceEnum (
    ..
    , ComputeType
    , RootVolume
    , UserVolume
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ModificationResourceEnum = ModificationResourceEnum' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern ComputeType :: ModificationResourceEnum
pattern ComputeType = ModificationResourceEnum' "COMPUTE_TYPE"

pattern RootVolume :: ModificationResourceEnum
pattern RootVolume = ModificationResourceEnum' "ROOT_VOLUME"

pattern UserVolume :: ModificationResourceEnum
pattern UserVolume = ModificationResourceEnum' "USER_VOLUME"

{-# COMPLETE
  ComputeType,
  RootVolume,
  UserVolume,
  ModificationResourceEnum' #-}

instance FromText ModificationResourceEnum where
    parser = (ModificationResourceEnum' . mk) <$> takeText

instance ToText ModificationResourceEnum where
    toText (ModificationResourceEnum' ci) = original ci

-- | Represents an enum of /known/ $ModificationResourceEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ModificationResourceEnum where
    toEnum i = case i of
        0 -> ComputeType
        1 -> RootVolume
        2 -> UserVolume
        _ -> (error . showText) $ "Unknown index for ModificationResourceEnum: " <> toText i
    fromEnum x = case x of
        ComputeType -> 0
        RootVolume -> 1
        UserVolume -> 2
        ModificationResourceEnum' name -> (error . showText) $ "Unknown ModificationResourceEnum: " <> original name

-- | Represents the bounds of /known/ $ModificationResourceEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ModificationResourceEnum where
    minBound = ComputeType
    maxBound = UserVolume

instance Hashable     ModificationResourceEnum
instance NFData       ModificationResourceEnum
instance ToByteString ModificationResourceEnum
instance ToQuery      ModificationResourceEnum
instance ToHeader     ModificationResourceEnum

instance FromJSON ModificationResourceEnum where
    parseJSON = parseJSONText "ModificationResourceEnum"
