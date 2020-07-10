{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.SchemaElement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types.SchemaElement (
  SchemaElement (
    ..
    , Resources
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Whether or not AWS includes resource IDs in the report. 
--
--
data SchemaElement = SchemaElement' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Resources :: SchemaElement
pattern Resources = SchemaElement' "RESOURCES"

{-# COMPLETE
  Resources,
  SchemaElement' #-}

instance FromText SchemaElement where
    parser = (SchemaElement' . mk) <$> takeText

instance ToText SchemaElement where
    toText (SchemaElement' ci) = original ci

-- | Represents an enum of /known/ $SchemaElement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SchemaElement where
    toEnum i = case i of
        0 -> Resources
        _ -> (error . showText) $ "Unknown index for SchemaElement: " <> toText i
    fromEnum x = case x of
        Resources -> 0
        SchemaElement' name -> (error . showText) $ "Unknown SchemaElement: " <> original name

-- | Represents the bounds of /known/ $SchemaElement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SchemaElement where
    minBound = Resources
    maxBound = Resources

instance Hashable     SchemaElement
instance NFData       SchemaElement
instance ToByteString SchemaElement
instance ToQuery      SchemaElement
instance ToHeader     SchemaElement

instance ToJSON SchemaElement where
    toJSON = toJSONText

instance FromJSON SchemaElement where
    parseJSON = parseJSONText "SchemaElement"
