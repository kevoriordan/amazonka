{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReturnValue (
  ReturnValue (
    ..
    , RVAllNew
    , RVAllOld
    , RVNone
    , RVUpdatedNew
    , RVUpdatedOld
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReturnValue = ReturnValue' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern RVAllNew :: ReturnValue
pattern RVAllNew = ReturnValue' "ALL_NEW"

pattern RVAllOld :: ReturnValue
pattern RVAllOld = ReturnValue' "ALL_OLD"

pattern RVNone :: ReturnValue
pattern RVNone = ReturnValue' "NONE"

pattern RVUpdatedNew :: ReturnValue
pattern RVUpdatedNew = ReturnValue' "UPDATED_NEW"

pattern RVUpdatedOld :: ReturnValue
pattern RVUpdatedOld = ReturnValue' "UPDATED_OLD"

{-# COMPLETE
  RVAllNew,
  RVAllOld,
  RVNone,
  RVUpdatedNew,
  RVUpdatedOld,
  ReturnValue' #-}

instance FromText ReturnValue where
    parser = (ReturnValue' . mk) <$> takeText

instance ToText ReturnValue where
    toText (ReturnValue' ci) = original ci

-- | Represents an enum of /known/ $ReturnValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReturnValue where
    toEnum i = case i of
        0 -> RVAllNew
        1 -> RVAllOld
        2 -> RVNone
        3 -> RVUpdatedNew
        4 -> RVUpdatedOld
        _ -> (error . showText) $ "Unknown index for ReturnValue: " <> toText i
    fromEnum x = case x of
        RVAllNew -> 0
        RVAllOld -> 1
        RVNone -> 2
        RVUpdatedNew -> 3
        RVUpdatedOld -> 4
        ReturnValue' name -> (error . showText) $ "Unknown ReturnValue: " <> original name

-- | Represents the bounds of /known/ $ReturnValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReturnValue where
    minBound = RVAllNew
    maxBound = RVUpdatedOld

instance Hashable     ReturnValue
instance NFData       ReturnValue
instance ToByteString ReturnValue
instance ToQuery      ReturnValue
instance ToHeader     ReturnValue

instance ToJSON ReturnValue where
    toJSON = toJSONText
