{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth (
  EnvironmentHealth (
    ..
    , Green
    , Grey
    , Red
    , Yellow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnvironmentHealth = EnvironmentHealth' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Green :: EnvironmentHealth
pattern Green = EnvironmentHealth' "Green"

pattern Grey :: EnvironmentHealth
pattern Grey = EnvironmentHealth' "Grey"

pattern Red :: EnvironmentHealth
pattern Red = EnvironmentHealth' "Red"

pattern Yellow :: EnvironmentHealth
pattern Yellow = EnvironmentHealth' "Yellow"

{-# COMPLETE
  Green,
  Grey,
  Red,
  Yellow,
  EnvironmentHealth' #-}

instance FromText EnvironmentHealth where
    parser = (EnvironmentHealth' . mk) <$> takeText

instance ToText EnvironmentHealth where
    toText (EnvironmentHealth' ci) = original ci

-- | Represents an enum of /known/ $EnvironmentHealth.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EnvironmentHealth where
    toEnum i = case i of
        0 -> Green
        1 -> Grey
        2 -> Red
        3 -> Yellow
        _ -> (error . showText) $ "Unknown index for EnvironmentHealth: " <> toText i
    fromEnum x = case x of
        Green -> 0
        Grey -> 1
        Red -> 2
        Yellow -> 3
        EnvironmentHealth' name -> (error . showText) $ "Unknown EnvironmentHealth: " <> original name

-- | Represents the bounds of /known/ $EnvironmentHealth.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnvironmentHealth where
    minBound = Green
    maxBound = Yellow

instance Hashable     EnvironmentHealth
instance NFData       EnvironmentHealth
instance ToByteString EnvironmentHealth
instance ToQuery      EnvironmentHealth
instance ToHeader     EnvironmentHealth

instance FromXML EnvironmentHealth where
    parseXML = parseXMLText "EnvironmentHealth"
