{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ServiceField
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ServiceField (
  ServiceField (
    ..
    , SFTags
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ServiceField = ServiceField' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern SFTags :: ServiceField
pattern SFTags = ServiceField' "TAGS"

{-# COMPLETE
  SFTags,
  ServiceField' #-}

instance FromText ServiceField where
    parser = (ServiceField' . mk) <$> takeText

instance ToText ServiceField where
    toText (ServiceField' ci) = original ci

-- | Represents an enum of /known/ $ServiceField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ServiceField where
    toEnum i = case i of
        0 -> SFTags
        _ -> (error . showText) $ "Unknown index for ServiceField: " <> toText i
    fromEnum x = case x of
        SFTags -> 0
        ServiceField' name -> (error . showText) $ "Unknown ServiceField: " <> original name

-- | Represents the bounds of /known/ $ServiceField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ServiceField where
    minBound = SFTags
    maxBound = SFTags

instance Hashable     ServiceField
instance NFData       ServiceField
instance ToByteString ServiceField
instance ToQuery      ServiceField
instance ToHeader     ServiceField

instance ToJSON ServiceField where
    toJSON = toJSONText
