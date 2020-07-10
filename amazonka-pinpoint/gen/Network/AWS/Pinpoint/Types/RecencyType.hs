{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RecencyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.RecencyType (
  RecencyType (
    ..
    , Active
    , Inactive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RecencyType = RecencyType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Active :: RecencyType
pattern Active = RecencyType' "ACTIVE"

pattern Inactive :: RecencyType
pattern Inactive = RecencyType' "INACTIVE"

{-# COMPLETE
  Active,
  Inactive,
  RecencyType' #-}

instance FromText RecencyType where
    parser = (RecencyType' . mk) <$> takeText

instance ToText RecencyType where
    toText (RecencyType' ci) = original ci

-- | Represents an enum of /known/ $RecencyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RecencyType where
    toEnum i = case i of
        0 -> Active
        1 -> Inactive
        _ -> (error . showText) $ "Unknown index for RecencyType: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Inactive -> 1
        RecencyType' name -> (error . showText) $ "Unknown RecencyType: " <> original name

-- | Represents the bounds of /known/ $RecencyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RecencyType where
    minBound = Active
    maxBound = Inactive

instance Hashable     RecencyType
instance NFData       RecencyType
instance ToByteString RecencyType
instance ToQuery      RecencyType
instance ToHeader     RecencyType

instance ToJSON RecencyType where
    toJSON = toJSONText

instance FromJSON RecencyType where
    parseJSON = parseJSONText "RecencyType"
