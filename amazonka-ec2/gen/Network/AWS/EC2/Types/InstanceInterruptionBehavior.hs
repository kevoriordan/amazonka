{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceInterruptionBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceInterruptionBehavior (
  InstanceInterruptionBehavior (
    ..
    , Hibernate
    , Stop
    , Terminate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceInterruptionBehavior = InstanceInterruptionBehavior' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern Hibernate :: InstanceInterruptionBehavior
pattern Hibernate = InstanceInterruptionBehavior' "hibernate"

pattern Stop :: InstanceInterruptionBehavior
pattern Stop = InstanceInterruptionBehavior' "stop"

pattern Terminate :: InstanceInterruptionBehavior
pattern Terminate = InstanceInterruptionBehavior' "terminate"

{-# COMPLETE
  Hibernate,
  Stop,
  Terminate,
  InstanceInterruptionBehavior' #-}

instance FromText InstanceInterruptionBehavior where
    parser = (InstanceInterruptionBehavior' . mk) <$> takeText

instance ToText InstanceInterruptionBehavior where
    toText (InstanceInterruptionBehavior' ci) = original ci

-- | Represents an enum of /known/ $InstanceInterruptionBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceInterruptionBehavior where
    toEnum i = case i of
        0 -> Hibernate
        1 -> Stop
        2 -> Terminate
        _ -> (error . showText) $ "Unknown index for InstanceInterruptionBehavior: " <> toText i
    fromEnum x = case x of
        Hibernate -> 0
        Stop -> 1
        Terminate -> 2
        InstanceInterruptionBehavior' name -> (error . showText) $ "Unknown InstanceInterruptionBehavior: " <> original name

-- | Represents the bounds of /known/ $InstanceInterruptionBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceInterruptionBehavior where
    minBound = Hibernate
    maxBound = Terminate

instance Hashable     InstanceInterruptionBehavior
instance NFData       InstanceInterruptionBehavior
instance ToByteString InstanceInterruptionBehavior
instance ToQuery      InstanceInterruptionBehavior
instance ToHeader     InstanceInterruptionBehavior

instance FromXML InstanceInterruptionBehavior where
    parseXML = parseXMLText "InstanceInterruptionBehavior"
