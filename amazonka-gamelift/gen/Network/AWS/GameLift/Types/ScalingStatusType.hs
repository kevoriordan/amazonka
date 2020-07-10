{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ScalingStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.ScalingStatusType (
  ScalingStatusType (
    ..
    , Active
    , DeleteRequested
    , Deleted
    , Deleting
    , Error'
    , UpdateRequested
    , Updating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScalingStatusType = ScalingStatusType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Active :: ScalingStatusType
pattern Active = ScalingStatusType' "ACTIVE"

pattern DeleteRequested :: ScalingStatusType
pattern DeleteRequested = ScalingStatusType' "DELETE_REQUESTED"

pattern Deleted :: ScalingStatusType
pattern Deleted = ScalingStatusType' "DELETED"

pattern Deleting :: ScalingStatusType
pattern Deleting = ScalingStatusType' "DELETING"

pattern Error' :: ScalingStatusType
pattern Error' = ScalingStatusType' "ERROR"

pattern UpdateRequested :: ScalingStatusType
pattern UpdateRequested = ScalingStatusType' "UPDATE_REQUESTED"

pattern Updating :: ScalingStatusType
pattern Updating = ScalingStatusType' "UPDATING"

{-# COMPLETE
  Active,
  DeleteRequested,
  Deleted,
  Deleting,
  Error',
  UpdateRequested,
  Updating,
  ScalingStatusType' #-}

instance FromText ScalingStatusType where
    parser = (ScalingStatusType' . mk) <$> takeText

instance ToText ScalingStatusType where
    toText (ScalingStatusType' ci) = original ci

-- | Represents an enum of /known/ $ScalingStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScalingStatusType where
    toEnum i = case i of
        0 -> Active
        1 -> DeleteRequested
        2 -> Deleted
        3 -> Deleting
        4 -> Error'
        5 -> UpdateRequested
        6 -> Updating
        _ -> (error . showText) $ "Unknown index for ScalingStatusType: " <> toText i
    fromEnum x = case x of
        Active -> 0
        DeleteRequested -> 1
        Deleted -> 2
        Deleting -> 3
        Error' -> 4
        UpdateRequested -> 5
        Updating -> 6
        ScalingStatusType' name -> (error . showText) $ "Unknown ScalingStatusType: " <> original name

-- | Represents the bounds of /known/ $ScalingStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScalingStatusType where
    minBound = Active
    maxBound = Updating

instance Hashable     ScalingStatusType
instance NFData       ScalingStatusType
instance ToByteString ScalingStatusType
instance ToQuery      ScalingStatusType
instance ToHeader     ScalingStatusType

instance ToJSON ScalingStatusType where
    toJSON = toJSONText

instance FromJSON ScalingStatusType where
    parseJSON = parseJSONText "ScalingStatusType"
