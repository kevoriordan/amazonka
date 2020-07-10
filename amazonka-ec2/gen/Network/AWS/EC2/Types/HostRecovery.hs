{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostRecovery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.HostRecovery (
  HostRecovery (
    ..
    , HRON
    , HROff
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data HostRecovery = HostRecovery' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern HRON :: HostRecovery
pattern HRON = HostRecovery' "on"

pattern HROff :: HostRecovery
pattern HROff = HostRecovery' "off"

{-# COMPLETE
  HRON,
  HROff,
  HostRecovery' #-}

instance FromText HostRecovery where
    parser = (HostRecovery' . mk) <$> takeText

instance ToText HostRecovery where
    toText (HostRecovery' ci) = original ci

-- | Represents an enum of /known/ $HostRecovery.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HostRecovery where
    toEnum i = case i of
        0 -> HRON
        1 -> HROff
        _ -> (error . showText) $ "Unknown index for HostRecovery: " <> toText i
    fromEnum x = case x of
        HRON -> 0
        HROff -> 1
        HostRecovery' name -> (error . showText) $ "Unknown HostRecovery: " <> original name

-- | Represents the bounds of /known/ $HostRecovery.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HostRecovery where
    minBound = HRON
    maxBound = HROff

instance Hashable     HostRecovery
instance NFData       HostRecovery
instance ToByteString HostRecovery
instance ToQuery      HostRecovery
instance ToHeader     HostRecovery

instance FromXML HostRecovery where
    parseXML = parseXMLText "HostRecovery"
