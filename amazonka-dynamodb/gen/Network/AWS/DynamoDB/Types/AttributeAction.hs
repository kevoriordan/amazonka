{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AttributeAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.AttributeAction (
  AttributeAction (
    ..
    , Add
    , Delete
    , Put
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AttributeAction = AttributeAction' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Add :: AttributeAction
pattern Add = AttributeAction' "ADD"

pattern Delete :: AttributeAction
pattern Delete = AttributeAction' "DELETE"

pattern Put :: AttributeAction
pattern Put = AttributeAction' "PUT"

{-# COMPLETE
  Add,
  Delete,
  Put,
  AttributeAction' #-}

instance FromText AttributeAction where
    parser = (AttributeAction' . mk) <$> takeText

instance ToText AttributeAction where
    toText (AttributeAction' ci) = original ci

-- | Represents an enum of /known/ $AttributeAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AttributeAction where
    toEnum i = case i of
        0 -> Add
        1 -> Delete
        2 -> Put
        _ -> (error . showText) $ "Unknown index for AttributeAction: " <> toText i
    fromEnum x = case x of
        Add -> 0
        Delete -> 1
        Put -> 2
        AttributeAction' name -> (error . showText) $ "Unknown AttributeAction: " <> original name

-- | Represents the bounds of /known/ $AttributeAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AttributeAction where
    minBound = Add
    maxBound = Put

instance Hashable     AttributeAction
instance NFData       AttributeAction
instance ToByteString AttributeAction
instance ToQuery      AttributeAction
instance ToHeader     AttributeAction

instance ToJSON AttributeAction where
    toJSON = toJSONText
