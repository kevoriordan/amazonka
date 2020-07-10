{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.AutoScalingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.AutoScalingType (
  AutoScalingType (
    ..
    , Load
    , Timer
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoScalingType = AutoScalingType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Load :: AutoScalingType
pattern Load = AutoScalingType' "load"

pattern Timer :: AutoScalingType
pattern Timer = AutoScalingType' "timer"

{-# COMPLETE
  Load,
  Timer,
  AutoScalingType' #-}

instance FromText AutoScalingType where
    parser = (AutoScalingType' . mk) <$> takeText

instance ToText AutoScalingType where
    toText (AutoScalingType' ci) = original ci

-- | Represents an enum of /known/ $AutoScalingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoScalingType where
    toEnum i = case i of
        0 -> Load
        1 -> Timer
        _ -> (error . showText) $ "Unknown index for AutoScalingType: " <> toText i
    fromEnum x = case x of
        Load -> 0
        Timer -> 1
        AutoScalingType' name -> (error . showText) $ "Unknown AutoScalingType: " <> original name

-- | Represents the bounds of /known/ $AutoScalingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoScalingType where
    minBound = Load
    maxBound = Timer

instance Hashable     AutoScalingType
instance NFData       AutoScalingType
instance ToByteString AutoScalingType
instance ToQuery      AutoScalingType
instance ToHeader     AutoScalingType

instance ToJSON AutoScalingType where
    toJSON = toJSONText

instance FromJSON AutoScalingType where
    parseJSON = parseJSONText "AutoScalingType"
