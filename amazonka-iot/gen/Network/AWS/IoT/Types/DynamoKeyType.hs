{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DynamoKeyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.DynamoKeyType (
  DynamoKeyType (
    ..
    , DKTNumber
    , DKTString
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DynamoKeyType = DynamoKeyType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern DKTNumber :: DynamoKeyType
pattern DKTNumber = DynamoKeyType' "NUMBER"

pattern DKTString :: DynamoKeyType
pattern DKTString = DynamoKeyType' "STRING"

{-# COMPLETE
  DKTNumber,
  DKTString,
  DynamoKeyType' #-}

instance FromText DynamoKeyType where
    parser = (DynamoKeyType' . mk) <$> takeText

instance ToText DynamoKeyType where
    toText (DynamoKeyType' ci) = original ci

-- | Represents an enum of /known/ $DynamoKeyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DynamoKeyType where
    toEnum i = case i of
        0 -> DKTNumber
        1 -> DKTString
        _ -> (error . showText) $ "Unknown index for DynamoKeyType: " <> toText i
    fromEnum x = case x of
        DKTNumber -> 0
        DKTString -> 1
        DynamoKeyType' name -> (error . showText) $ "Unknown DynamoKeyType: " <> original name

-- | Represents the bounds of /known/ $DynamoKeyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DynamoKeyType where
    minBound = DKTNumber
    maxBound = DKTString

instance Hashable     DynamoKeyType
instance NFData       DynamoKeyType
instance ToByteString DynamoKeyType
instance ToQuery      DynamoKeyType
instance ToHeader     DynamoKeyType

instance ToJSON DynamoKeyType where
    toJSON = toJSONText

instance FromJSON DynamoKeyType where
    parseJSON = parseJSONText "DynamoKeyType"
