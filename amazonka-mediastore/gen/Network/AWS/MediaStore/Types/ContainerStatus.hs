{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.ContainerStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStore.Types.ContainerStatus (
  ContainerStatus (
    ..
    , Active
    , Creating
    , Deleting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContainerStatus = ContainerStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Active :: ContainerStatus
pattern Active = ContainerStatus' "ACTIVE"

pattern Creating :: ContainerStatus
pattern Creating = ContainerStatus' "CREATING"

pattern Deleting :: ContainerStatus
pattern Deleting = ContainerStatus' "DELETING"

{-# COMPLETE
  Active,
  Creating,
  Deleting,
  ContainerStatus' #-}

instance FromText ContainerStatus where
    parser = (ContainerStatus' . mk) <$> takeText

instance ToText ContainerStatus where
    toText (ContainerStatus' ci) = original ci

-- | Represents an enum of /known/ $ContainerStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContainerStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Creating
        2 -> Deleting
        _ -> (error . showText) $ "Unknown index for ContainerStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Creating -> 1
        Deleting -> 2
        ContainerStatus' name -> (error . showText) $ "Unknown ContainerStatus: " <> original name

-- | Represents the bounds of /known/ $ContainerStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContainerStatus where
    minBound = Active
    maxBound = Deleting

instance Hashable     ContainerStatus
instance NFData       ContainerStatus
instance ToByteString ContainerStatus
instance ToQuery      ContainerStatus
instance ToHeader     ContainerStatus

instance FromJSON ContainerStatus where
    parseJSON = parseJSONText "ContainerStatus"
