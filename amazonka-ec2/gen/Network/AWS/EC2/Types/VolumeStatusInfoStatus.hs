{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusInfoStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeStatusInfoStatus (
  VolumeStatusInfoStatus (
    ..
    , Impaired
    , InsufficientData
    , OK
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeStatusInfoStatus = VolumeStatusInfoStatus' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Impaired :: VolumeStatusInfoStatus
pattern Impaired = VolumeStatusInfoStatus' "impaired"

pattern InsufficientData :: VolumeStatusInfoStatus
pattern InsufficientData = VolumeStatusInfoStatus' "insufficient-data"

pattern OK :: VolumeStatusInfoStatus
pattern OK = VolumeStatusInfoStatus' "ok"

{-# COMPLETE
  Impaired,
  InsufficientData,
  OK,
  VolumeStatusInfoStatus' #-}

instance FromText VolumeStatusInfoStatus where
    parser = (VolumeStatusInfoStatus' . mk) <$> takeText

instance ToText VolumeStatusInfoStatus where
    toText (VolumeStatusInfoStatus' ci) = original ci

-- | Represents an enum of /known/ $VolumeStatusInfoStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VolumeStatusInfoStatus where
    toEnum i = case i of
        0 -> Impaired
        1 -> InsufficientData
        2 -> OK
        _ -> (error . showText) $ "Unknown index for VolumeStatusInfoStatus: " <> toText i
    fromEnum x = case x of
        Impaired -> 0
        InsufficientData -> 1
        OK -> 2
        VolumeStatusInfoStatus' name -> (error . showText) $ "Unknown VolumeStatusInfoStatus: " <> original name

-- | Represents the bounds of /known/ $VolumeStatusInfoStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VolumeStatusInfoStatus where
    minBound = Impaired
    maxBound = OK

instance Hashable     VolumeStatusInfoStatus
instance NFData       VolumeStatusInfoStatus
instance ToByteString VolumeStatusInfoStatus
instance ToQuery      VolumeStatusInfoStatus
instance ToHeader     VolumeStatusInfoStatus

instance FromXML VolumeStatusInfoStatus where
    parseXML = parseXMLText "VolumeStatusInfoStatus"
