{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.PropertyKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.PropertyKey (
  PropertyKey (
    ..
    , Owner
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PropertyKey = PropertyKey' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Owner :: PropertyKey
pattern Owner = PropertyKey' "OWNER"

{-# COMPLETE
  Owner,
  PropertyKey' #-}

instance FromText PropertyKey where
    parser = (PropertyKey' . mk) <$> takeText

instance ToText PropertyKey where
    toText (PropertyKey' ci) = original ci

-- | Represents an enum of /known/ $PropertyKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PropertyKey where
    toEnum i = case i of
        0 -> Owner
        _ -> (error . showText) $ "Unknown index for PropertyKey: " <> toText i
    fromEnum x = case x of
        Owner -> 0
        PropertyKey' name -> (error . showText) $ "Unknown PropertyKey: " <> original name

-- | Represents the bounds of /known/ $PropertyKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PropertyKey where
    minBound = Owner
    maxBound = Owner

instance Hashable     PropertyKey
instance NFData       PropertyKey
instance ToByteString PropertyKey
instance ToQuery      PropertyKey
instance ToHeader     PropertyKey

instance ToJSON PropertyKey where
    toJSON = toJSONText

instance FromJSON PropertyKey where
    parseJSON = parseJSONText "PropertyKey"
