{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior (
  SpotInstanceInterruptionBehavior (
    ..
    , SIIBHibernate
    , SIIBStop
    , SIIBTerminate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SpotInstanceInterruptionBehavior = SpotInstanceInterruptionBehavior' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern SIIBHibernate :: SpotInstanceInterruptionBehavior
pattern SIIBHibernate = SpotInstanceInterruptionBehavior' "hibernate"

pattern SIIBStop :: SpotInstanceInterruptionBehavior
pattern SIIBStop = SpotInstanceInterruptionBehavior' "stop"

pattern SIIBTerminate :: SpotInstanceInterruptionBehavior
pattern SIIBTerminate = SpotInstanceInterruptionBehavior' "terminate"

{-# COMPLETE
  SIIBHibernate,
  SIIBStop,
  SIIBTerminate,
  SpotInstanceInterruptionBehavior' #-}

instance FromText SpotInstanceInterruptionBehavior where
    parser = (SpotInstanceInterruptionBehavior' . mk) <$> takeText

instance ToText SpotInstanceInterruptionBehavior where
    toText (SpotInstanceInterruptionBehavior' ci) = original ci

-- | Represents an enum of /known/ $SpotInstanceInterruptionBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SpotInstanceInterruptionBehavior where
    toEnum i = case i of
        0 -> SIIBHibernate
        1 -> SIIBStop
        2 -> SIIBTerminate
        _ -> (error . showText) $ "Unknown index for SpotInstanceInterruptionBehavior: " <> toText i
    fromEnum x = case x of
        SIIBHibernate -> 0
        SIIBStop -> 1
        SIIBTerminate -> 2
        SpotInstanceInterruptionBehavior' name -> (error . showText) $ "Unknown SpotInstanceInterruptionBehavior: " <> original name

-- | Represents the bounds of /known/ $SpotInstanceInterruptionBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SpotInstanceInterruptionBehavior where
    minBound = SIIBHibernate
    maxBound = SIIBTerminate

instance Hashable     SpotInstanceInterruptionBehavior
instance NFData       SpotInstanceInterruptionBehavior
instance ToByteString SpotInstanceInterruptionBehavior
instance ToQuery      SpotInstanceInterruptionBehavior
instance ToHeader     SpotInstanceInterruptionBehavior

instance FromXML SpotInstanceInterruptionBehavior where
    parseXML = parseXMLText "SpotInstanceInterruptionBehavior"
