{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AddressStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AddressStatus (
  AddressStatus (
    ..
    , InClassic
    , InVPC
    , MoveInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AddressStatus = AddressStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern InClassic :: AddressStatus
pattern InClassic = AddressStatus' "InClassic"

pattern InVPC :: AddressStatus
pattern InVPC = AddressStatus' "InVpc"

pattern MoveInProgress :: AddressStatus
pattern MoveInProgress = AddressStatus' "MoveInProgress"

{-# COMPLETE
  InClassic,
  InVPC,
  MoveInProgress,
  AddressStatus' #-}

instance FromText AddressStatus where
    parser = (AddressStatus' . mk) <$> takeText

instance ToText AddressStatus where
    toText (AddressStatus' ci) = original ci

-- | Represents an enum of /known/ $AddressStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AddressStatus where
    toEnum i = case i of
        0 -> InClassic
        1 -> InVPC
        2 -> MoveInProgress
        _ -> (error . showText) $ "Unknown index for AddressStatus: " <> toText i
    fromEnum x = case x of
        InClassic -> 0
        InVPC -> 1
        MoveInProgress -> 2
        AddressStatus' name -> (error . showText) $ "Unknown AddressStatus: " <> original name

-- | Represents the bounds of /known/ $AddressStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AddressStatus where
    minBound = InClassic
    maxBound = MoveInProgress

instance Hashable     AddressStatus
instance NFData       AddressStatus
instance ToByteString AddressStatus
instance ToQuery      AddressStatus
instance ToHeader     AddressStatus

instance FromXML AddressStatus where
    parseXML = parseXMLText "AddressStatus"
