{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SubResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.SubResourceType (
  SubResourceType (
    ..
    , IP
    , URL
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SubResourceType = SubResourceType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern IP :: SubResourceType
pattern IP = SubResourceType' "IP"

pattern URL :: SubResourceType
pattern URL = SubResourceType' "URL"

{-# COMPLETE
  IP,
  URL,
  SubResourceType' #-}

instance FromText SubResourceType where
    parser = (SubResourceType' . mk) <$> takeText

instance ToText SubResourceType where
    toText (SubResourceType' ci) = original ci

-- | Represents an enum of /known/ $SubResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SubResourceType where
    toEnum i = case i of
        0 -> IP
        1 -> URL
        _ -> (error . showText) $ "Unknown index for SubResourceType: " <> toText i
    fromEnum x = case x of
        IP -> 0
        URL -> 1
        SubResourceType' name -> (error . showText) $ "Unknown SubResourceType: " <> original name

-- | Represents the bounds of /known/ $SubResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SubResourceType where
    minBound = IP
    maxBound = URL

instance Hashable     SubResourceType
instance NFData       SubResourceType
instance ToByteString SubResourceType
instance ToQuery      SubResourceType
instance ToHeader     SubResourceType

instance FromJSON SubResourceType where
    parseJSON = parseJSONText "SubResourceType"
