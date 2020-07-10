{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.ResourceType (
  ResourceType (
    ..
    , Gpu
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceType = ResourceType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Gpu :: ResourceType
pattern Gpu = ResourceType' "GPU"

{-# COMPLETE
  Gpu,
  ResourceType' #-}

instance FromText ResourceType where
    parser = (ResourceType' . mk) <$> takeText

instance ToText ResourceType where
    toText (ResourceType' ci) = original ci

-- | Represents an enum of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceType where
    toEnum i = case i of
        0 -> Gpu
        _ -> (error . showText) $ "Unknown index for ResourceType: " <> toText i
    fromEnum x = case x of
        Gpu -> 0
        ResourceType' name -> (error . showText) $ "Unknown ResourceType: " <> original name

-- | Represents the bounds of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceType where
    minBound = Gpu
    maxBound = Gpu

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"
