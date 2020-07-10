{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OpsItemStatus (
  OpsItemStatus (
    ..
    , InProgress
    , Open
    , Resolved
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OpsItemStatus = OpsItemStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern InProgress :: OpsItemStatus
pattern InProgress = OpsItemStatus' "InProgress"

pattern Open :: OpsItemStatus
pattern Open = OpsItemStatus' "Open"

pattern Resolved :: OpsItemStatus
pattern Resolved = OpsItemStatus' "Resolved"

{-# COMPLETE
  InProgress,
  Open,
  Resolved,
  OpsItemStatus' #-}

instance FromText OpsItemStatus where
    parser = (OpsItemStatus' . mk) <$> takeText

instance ToText OpsItemStatus where
    toText (OpsItemStatus' ci) = original ci

-- | Represents an enum of /known/ $OpsItemStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OpsItemStatus where
    toEnum i = case i of
        0 -> InProgress
        1 -> Open
        2 -> Resolved
        _ -> (error . showText) $ "Unknown index for OpsItemStatus: " <> toText i
    fromEnum x = case x of
        InProgress -> 0
        Open -> 1
        Resolved -> 2
        OpsItemStatus' name -> (error . showText) $ "Unknown OpsItemStatus: " <> original name

-- | Represents the bounds of /known/ $OpsItemStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OpsItemStatus where
    minBound = InProgress
    maxBound = Resolved

instance Hashable     OpsItemStatus
instance NFData       OpsItemStatus
instance ToByteString OpsItemStatus
instance ToQuery      OpsItemStatus
instance ToHeader     OpsItemStatus

instance ToJSON OpsItemStatus where
    toJSON = toJSONText

instance FromJSON OpsItemStatus where
    parseJSON = parseJSONText "OpsItemStatus"
