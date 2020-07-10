{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProviderField
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.CapacityProviderField (
  CapacityProviderField (
    ..
    , CPFTags
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CapacityProviderField = CapacityProviderField' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern CPFTags :: CapacityProviderField
pattern CPFTags = CapacityProviderField' "TAGS"

{-# COMPLETE
  CPFTags,
  CapacityProviderField' #-}

instance FromText CapacityProviderField where
    parser = (CapacityProviderField' . mk) <$> takeText

instance ToText CapacityProviderField where
    toText (CapacityProviderField' ci) = original ci

-- | Represents an enum of /known/ $CapacityProviderField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CapacityProviderField where
    toEnum i = case i of
        0 -> CPFTags
        _ -> (error . showText) $ "Unknown index for CapacityProviderField: " <> toText i
    fromEnum x = case x of
        CPFTags -> 0
        CapacityProviderField' name -> (error . showText) $ "Unknown CapacityProviderField: " <> original name

-- | Represents the bounds of /known/ $CapacityProviderField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CapacityProviderField where
    minBound = CPFTags
    maxBound = CPFTags

instance Hashable     CapacityProviderField
instance NFData       CapacityProviderField
instance ToByteString CapacityProviderField
instance ToQuery      CapacityProviderField
instance ToHeader     CapacityProviderField

instance ToJSON CapacityProviderField where
    toJSON = toJSONText
