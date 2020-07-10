{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MoveStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.MoveStatus (
  MoveStatus (
    ..
    , MovingToVPC
    , RestoringToClassic
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data MoveStatus = MoveStatus' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern MovingToVPC :: MoveStatus
pattern MovingToVPC = MoveStatus' "movingToVpc"

pattern RestoringToClassic :: MoveStatus
pattern RestoringToClassic = MoveStatus' "restoringToClassic"

{-# COMPLETE
  MovingToVPC,
  RestoringToClassic,
  MoveStatus' #-}

instance FromText MoveStatus where
    parser = (MoveStatus' . mk) <$> takeText

instance ToText MoveStatus where
    toText (MoveStatus' ci) = original ci

-- | Represents an enum of /known/ $MoveStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MoveStatus where
    toEnum i = case i of
        0 -> MovingToVPC
        1 -> RestoringToClassic
        _ -> (error . showText) $ "Unknown index for MoveStatus: " <> toText i
    fromEnum x = case x of
        MovingToVPC -> 0
        RestoringToClassic -> 1
        MoveStatus' name -> (error . showText) $ "Unknown MoveStatus: " <> original name

-- | Represents the bounds of /known/ $MoveStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MoveStatus where
    minBound = MovingToVPC
    maxBound = RestoringToClassic

instance Hashable     MoveStatus
instance NFData       MoveStatus
instance ToByteString MoveStatus
instance ToQuery      MoveStatus
instance ToHeader     MoveStatus

instance FromXML MoveStatus where
    parseXML = parseXMLText "MoveStatus"
