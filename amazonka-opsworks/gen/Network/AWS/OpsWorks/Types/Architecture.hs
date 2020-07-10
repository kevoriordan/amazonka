{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Architecture
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Architecture (
  Architecture (
    ..
    , I386
    , X86_64
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Architecture = Architecture' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern I386 :: Architecture
pattern I386 = Architecture' "i386"

pattern X86_64 :: Architecture
pattern X86_64 = Architecture' "x86_64"

{-# COMPLETE
  I386,
  X86_64,
  Architecture' #-}

instance FromText Architecture where
    parser = (Architecture' . mk) <$> takeText

instance ToText Architecture where
    toText (Architecture' ci) = original ci

-- | Represents an enum of /known/ $Architecture.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Architecture where
    toEnum i = case i of
        0 -> I386
        1 -> X86_64
        _ -> (error . showText) $ "Unknown index for Architecture: " <> toText i
    fromEnum x = case x of
        I386 -> 0
        X86_64 -> 1
        Architecture' name -> (error . showText) $ "Unknown Architecture: " <> original name

-- | Represents the bounds of /known/ $Architecture.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Architecture where
    minBound = I386
    maxBound = X86_64

instance Hashable     Architecture
instance NFData       Architecture
instance ToByteString Architecture
instance ToQuery      Architecture
instance ToHeader     Architecture

instance ToJSON Architecture where
    toJSON = toJSONText

instance FromJSON Architecture where
    parseJSON = parseJSONText "Architecture"
