{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourceCollectionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.ResourceCollectionType (
  ResourceCollectionType (
    ..
    , SharedWithMe
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceCollectionType = ResourceCollectionType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern SharedWithMe :: ResourceCollectionType
pattern SharedWithMe = ResourceCollectionType' "SHARED_WITH_ME"

{-# COMPLETE
  SharedWithMe,
  ResourceCollectionType' #-}

instance FromText ResourceCollectionType where
    parser = (ResourceCollectionType' . mk) <$> takeText

instance ToText ResourceCollectionType where
    toText (ResourceCollectionType' ci) = original ci

-- | Represents an enum of /known/ $ResourceCollectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceCollectionType where
    toEnum i = case i of
        0 -> SharedWithMe
        _ -> (error . showText) $ "Unknown index for ResourceCollectionType: " <> toText i
    fromEnum x = case x of
        SharedWithMe -> 0
        ResourceCollectionType' name -> (error . showText) $ "Unknown ResourceCollectionType: " <> original name

-- | Represents the bounds of /known/ $ResourceCollectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceCollectionType where
    minBound = SharedWithMe
    maxBound = SharedWithMe

instance Hashable     ResourceCollectionType
instance NFData       ResourceCollectionType
instance ToByteString ResourceCollectionType
instance ToQuery      ResourceCollectionType
instance ToHeader     ResourceCollectionType

instance ToJSON ResourceCollectionType where
    toJSON = toJSONText
