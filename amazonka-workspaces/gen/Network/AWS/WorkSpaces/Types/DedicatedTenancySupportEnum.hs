{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.DedicatedTenancySupportEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.DedicatedTenancySupportEnum (
  DedicatedTenancySupportEnum (
    ..
    , DTSEEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DedicatedTenancySupportEnum = DedicatedTenancySupportEnum' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern DTSEEnabled :: DedicatedTenancySupportEnum
pattern DTSEEnabled = DedicatedTenancySupportEnum' "ENABLED"

{-# COMPLETE
  DTSEEnabled,
  DedicatedTenancySupportEnum' #-}

instance FromText DedicatedTenancySupportEnum where
    parser = (DedicatedTenancySupportEnum' . mk) <$> takeText

instance ToText DedicatedTenancySupportEnum where
    toText (DedicatedTenancySupportEnum' ci) = original ci

-- | Represents an enum of /known/ $DedicatedTenancySupportEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DedicatedTenancySupportEnum where
    toEnum i = case i of
        0 -> DTSEEnabled
        _ -> (error . showText) $ "Unknown index for DedicatedTenancySupportEnum: " <> toText i
    fromEnum x = case x of
        DTSEEnabled -> 0
        DedicatedTenancySupportEnum' name -> (error . showText) $ "Unknown DedicatedTenancySupportEnum: " <> original name

-- | Represents the bounds of /known/ $DedicatedTenancySupportEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DedicatedTenancySupportEnum where
    minBound = DTSEEnabled
    maxBound = DTSEEnabled

instance Hashable     DedicatedTenancySupportEnum
instance NFData       DedicatedTenancySupportEnum
instance ToByteString DedicatedTenancySupportEnum
instance ToQuery      DedicatedTenancySupportEnum
instance ToHeader     DedicatedTenancySupportEnum

instance ToJSON DedicatedTenancySupportEnum where
    toJSON = toJSONText
