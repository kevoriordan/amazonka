{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.UpdateTargetsArchitecture
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.UpdateTargetsArchitecture (
  UpdateTargetsArchitecture (
    ..
    , AARCH64
    , Armv6l
    , Armv7l
    , X86_64
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The architecture of the cores which are the targets of an update.
data UpdateTargetsArchitecture = UpdateTargetsArchitecture' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern AARCH64 :: UpdateTargetsArchitecture
pattern AARCH64 = UpdateTargetsArchitecture' "aarch64"

pattern Armv6l :: UpdateTargetsArchitecture
pattern Armv6l = UpdateTargetsArchitecture' "armv6l"

pattern Armv7l :: UpdateTargetsArchitecture
pattern Armv7l = UpdateTargetsArchitecture' "armv7l"

pattern X86_64 :: UpdateTargetsArchitecture
pattern X86_64 = UpdateTargetsArchitecture' "x86_64"

{-# COMPLETE
  AARCH64,
  Armv6l,
  Armv7l,
  X86_64,
  UpdateTargetsArchitecture' #-}

instance FromText UpdateTargetsArchitecture where
    parser = (UpdateTargetsArchitecture' . mk) <$> takeText

instance ToText UpdateTargetsArchitecture where
    toText (UpdateTargetsArchitecture' ci) = original ci

-- | Represents an enum of /known/ $UpdateTargetsArchitecture.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UpdateTargetsArchitecture where
    toEnum i = case i of
        0 -> AARCH64
        1 -> Armv6l
        2 -> Armv7l
        3 -> X86_64
        _ -> (error . showText) $ "Unknown index for UpdateTargetsArchitecture: " <> toText i
    fromEnum x = case x of
        AARCH64 -> 0
        Armv6l -> 1
        Armv7l -> 2
        X86_64 -> 3
        UpdateTargetsArchitecture' name -> (error . showText) $ "Unknown UpdateTargetsArchitecture: " <> original name

-- | Represents the bounds of /known/ $UpdateTargetsArchitecture.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UpdateTargetsArchitecture where
    minBound = AARCH64
    maxBound = X86_64

instance Hashable     UpdateTargetsArchitecture
instance NFData       UpdateTargetsArchitecture
instance ToByteString UpdateTargetsArchitecture
instance ToQuery      UpdateTargetsArchitecture
instance ToHeader     UpdateTargetsArchitecture

instance ToJSON UpdateTargetsArchitecture where
    toJSON = toJSONText
