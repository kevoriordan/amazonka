{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMatchCriteria
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceMatchCriteria (
  InstanceMatchCriteria (
    ..
    , IMCOpen
    , IMCTargeted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceMatchCriteria = InstanceMatchCriteria' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern IMCOpen :: InstanceMatchCriteria
pattern IMCOpen = InstanceMatchCriteria' "open"

pattern IMCTargeted :: InstanceMatchCriteria
pattern IMCTargeted = InstanceMatchCriteria' "targeted"

{-# COMPLETE
  IMCOpen,
  IMCTargeted,
  InstanceMatchCriteria' #-}

instance FromText InstanceMatchCriteria where
    parser = (InstanceMatchCriteria' . mk) <$> takeText

instance ToText InstanceMatchCriteria where
    toText (InstanceMatchCriteria' ci) = original ci

-- | Represents an enum of /known/ $InstanceMatchCriteria.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceMatchCriteria where
    toEnum i = case i of
        0 -> IMCOpen
        1 -> IMCTargeted
        _ -> (error . showText) $ "Unknown index for InstanceMatchCriteria: " <> toText i
    fromEnum x = case x of
        IMCOpen -> 0
        IMCTargeted -> 1
        InstanceMatchCriteria' name -> (error . showText) $ "Unknown InstanceMatchCriteria: " <> original name

-- | Represents the bounds of /known/ $InstanceMatchCriteria.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceMatchCriteria where
    minBound = IMCOpen
    maxBound = IMCTargeted

instance Hashable     InstanceMatchCriteria
instance NFData       InstanceMatchCriteria
instance ToByteString InstanceMatchCriteria
instance ToQuery      InstanceMatchCriteria
instance ToHeader     InstanceMatchCriteria

instance FromXML InstanceMatchCriteria where
    parseXML = parseXMLText "InstanceMatchCriteria"
