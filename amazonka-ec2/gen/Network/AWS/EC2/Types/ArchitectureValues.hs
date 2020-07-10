{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ArchitectureValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ArchitectureValues (
  ArchitectureValues (
    ..
    , ARM64
    , I386
    , X86_64
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ArchitectureValues = ArchitectureValues' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern ARM64 :: ArchitectureValues
pattern ARM64 = ArchitectureValues' "arm64"

pattern I386 :: ArchitectureValues
pattern I386 = ArchitectureValues' "i386"

pattern X86_64 :: ArchitectureValues
pattern X86_64 = ArchitectureValues' "x86_64"

{-# COMPLETE
  ARM64,
  I386,
  X86_64,
  ArchitectureValues' #-}

instance FromText ArchitectureValues where
    parser = (ArchitectureValues' . mk) <$> takeText

instance ToText ArchitectureValues where
    toText (ArchitectureValues' ci) = original ci

-- | Represents an enum of /known/ $ArchitectureValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ArchitectureValues where
    toEnum i = case i of
        0 -> ARM64
        1 -> I386
        2 -> X86_64
        _ -> (error . showText) $ "Unknown index for ArchitectureValues: " <> toText i
    fromEnum x = case x of
        ARM64 -> 0
        I386 -> 1
        X86_64 -> 2
        ArchitectureValues' name -> (error . showText) $ "Unknown ArchitectureValues: " <> original name

-- | Represents the bounds of /known/ $ArchitectureValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ArchitectureValues where
    minBound = ARM64
    maxBound = X86_64

instance Hashable     ArchitectureValues
instance NFData       ArchitectureValues
instance ToByteString ArchitectureValues
instance ToQuery      ArchitectureValues
instance ToHeader     ArchitectureValues

instance FromXML ArchitectureValues where
    parseXML = parseXMLText "ArchitectureValues"
