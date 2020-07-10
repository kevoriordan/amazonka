{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.ContentType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types.ContentType (
  ContentType (
    ..
    , ApplicationJSON
    , ApplicationXML
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContentType = ContentType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern ApplicationJSON :: ContentType
pattern ApplicationJSON = ContentType' "application/json"

pattern ApplicationXML :: ContentType
pattern ApplicationXML = ContentType' "application/xml"

{-# COMPLETE
  ApplicationJSON,
  ApplicationXML,
  ContentType' #-}

instance FromText ContentType where
    parser = (ContentType' . mk) <$> takeText

instance ToText ContentType where
    toText (ContentType' ci) = original ci

-- | Represents an enum of /known/ $ContentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContentType where
    toEnum i = case i of
        0 -> ApplicationJSON
        1 -> ApplicationXML
        _ -> (error . showText) $ "Unknown index for ContentType: " <> toText i
    fromEnum x = case x of
        ApplicationJSON -> 0
        ApplicationXML -> 1
        ContentType' name -> (error . showText) $ "Unknown ContentType: " <> original name

-- | Represents the bounds of /known/ $ContentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContentType where
    minBound = ApplicationJSON
    maxBound = ApplicationXML

instance Hashable     ContentType
instance NFData       ContentType
instance ToByteString ContentType
instance ToQuery      ContentType
instance ToHeader     ContentType

instance ToJSON ContentType where
    toJSON = toJSONText
