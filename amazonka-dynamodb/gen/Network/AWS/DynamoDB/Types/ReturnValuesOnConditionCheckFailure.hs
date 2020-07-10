{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure (
  ReturnValuesOnConditionCheckFailure (
    ..
    , AllOld
    , None
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReturnValuesOnConditionCheckFailure = ReturnValuesOnConditionCheckFailure' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern AllOld :: ReturnValuesOnConditionCheckFailure
pattern AllOld = ReturnValuesOnConditionCheckFailure' "ALL_OLD"

pattern None :: ReturnValuesOnConditionCheckFailure
pattern None = ReturnValuesOnConditionCheckFailure' "NONE"

{-# COMPLETE
  AllOld,
  None,
  ReturnValuesOnConditionCheckFailure' #-}

instance FromText ReturnValuesOnConditionCheckFailure where
    parser = (ReturnValuesOnConditionCheckFailure' . mk) <$> takeText

instance ToText ReturnValuesOnConditionCheckFailure where
    toText (ReturnValuesOnConditionCheckFailure' ci) = original ci

-- | Represents an enum of /known/ $ReturnValuesOnConditionCheckFailure.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReturnValuesOnConditionCheckFailure where
    toEnum i = case i of
        0 -> AllOld
        1 -> None
        _ -> (error . showText) $ "Unknown index for ReturnValuesOnConditionCheckFailure: " <> toText i
    fromEnum x = case x of
        AllOld -> 0
        None -> 1
        ReturnValuesOnConditionCheckFailure' name -> (error . showText) $ "Unknown ReturnValuesOnConditionCheckFailure: " <> original name

-- | Represents the bounds of /known/ $ReturnValuesOnConditionCheckFailure.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReturnValuesOnConditionCheckFailure where
    minBound = AllOld
    maxBound = None

instance Hashable     ReturnValuesOnConditionCheckFailure
instance NFData       ReturnValuesOnConditionCheckFailure
instance ToByteString ReturnValuesOnConditionCheckFailure
instance ToQuery      ReturnValuesOnConditionCheckFailure
instance ToHeader     ReturnValuesOnConditionCheckFailure

instance ToJSON ReturnValuesOnConditionCheckFailure where
    toJSON = toJSONText
