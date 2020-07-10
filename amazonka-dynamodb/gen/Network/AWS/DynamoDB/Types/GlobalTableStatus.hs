{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.GlobalTableStatus (
  GlobalTableStatus (
    ..
    , GTSActive
    , GTSCreating
    , GTSDeleting
    , GTSUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GlobalTableStatus = GlobalTableStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern GTSActive :: GlobalTableStatus
pattern GTSActive = GlobalTableStatus' "ACTIVE"

pattern GTSCreating :: GlobalTableStatus
pattern GTSCreating = GlobalTableStatus' "CREATING"

pattern GTSDeleting :: GlobalTableStatus
pattern GTSDeleting = GlobalTableStatus' "DELETING"

pattern GTSUpdating :: GlobalTableStatus
pattern GTSUpdating = GlobalTableStatus' "UPDATING"

{-# COMPLETE
  GTSActive,
  GTSCreating,
  GTSDeleting,
  GTSUpdating,
  GlobalTableStatus' #-}

instance FromText GlobalTableStatus where
    parser = (GlobalTableStatus' . mk) <$> takeText

instance ToText GlobalTableStatus where
    toText (GlobalTableStatus' ci) = original ci

-- | Represents an enum of /known/ $GlobalTableStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GlobalTableStatus where
    toEnum i = case i of
        0 -> GTSActive
        1 -> GTSCreating
        2 -> GTSDeleting
        3 -> GTSUpdating
        _ -> (error . showText) $ "Unknown index for GlobalTableStatus: " <> toText i
    fromEnum x = case x of
        GTSActive -> 0
        GTSCreating -> 1
        GTSDeleting -> 2
        GTSUpdating -> 3
        GlobalTableStatus' name -> (error . showText) $ "Unknown GlobalTableStatus: " <> original name

-- | Represents the bounds of /known/ $GlobalTableStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GlobalTableStatus where
    minBound = GTSActive
    maxBound = GTSUpdating

instance Hashable     GlobalTableStatus
instance NFData       GlobalTableStatus
instance ToByteString GlobalTableStatus
instance ToQuery      GlobalTableStatus
instance ToHeader     GlobalTableStatus

instance FromJSON GlobalTableStatus where
    parseJSON = parseJSONText "GlobalTableStatus"
