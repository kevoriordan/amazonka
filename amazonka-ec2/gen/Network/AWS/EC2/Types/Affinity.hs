{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Affinity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Affinity (
  Affinity (
    ..
    , ADefault
    , AHost
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data Affinity = Affinity' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern ADefault :: Affinity
pattern ADefault = Affinity' "default"

pattern AHost :: Affinity
pattern AHost = Affinity' "host"

{-# COMPLETE
  ADefault,
  AHost,
  Affinity' #-}

instance FromText Affinity where
    parser = (Affinity' . mk) <$> takeText

instance ToText Affinity where
    toText (Affinity' ci) = original ci

-- | Represents an enum of /known/ $Affinity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Affinity where
    toEnum i = case i of
        0 -> ADefault
        1 -> AHost
        _ -> (error . showText) $ "Unknown index for Affinity: " <> toText i
    fromEnum x = case x of
        ADefault -> 0
        AHost -> 1
        Affinity' name -> (error . showText) $ "Unknown Affinity: " <> original name

-- | Represents the bounds of /known/ $Affinity.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Affinity where
    minBound = ADefault
    maxBound = AHost

instance Hashable     Affinity
instance NFData       Affinity
instance ToByteString Affinity
instance ToQuery      Affinity
instance ToHeader     Affinity
