{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ArchitectureType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ArchitectureType (
  ArchitectureType (
    ..
    , ATARM64
    , ATI386
    , ATX86_64
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ArchitectureType = ArchitectureType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern ATARM64 :: ArchitectureType
pattern ATARM64 = ArchitectureType' "arm64"

pattern ATI386 :: ArchitectureType
pattern ATI386 = ArchitectureType' "i386"

pattern ATX86_64 :: ArchitectureType
pattern ATX86_64 = ArchitectureType' "x86_64"

{-# COMPLETE
  ATARM64,
  ATI386,
  ATX86_64,
  ArchitectureType' #-}

instance FromText ArchitectureType where
    parser = (ArchitectureType' . mk) <$> takeText

instance ToText ArchitectureType where
    toText (ArchitectureType' ci) = original ci

-- | Represents an enum of /known/ $ArchitectureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ArchitectureType where
    toEnum i = case i of
        0 -> ATARM64
        1 -> ATI386
        2 -> ATX86_64
        _ -> (error . showText) $ "Unknown index for ArchitectureType: " <> toText i
    fromEnum x = case x of
        ATARM64 -> 0
        ATI386 -> 1
        ATX86_64 -> 2
        ArchitectureType' name -> (error . showText) $ "Unknown ArchitectureType: " <> original name

-- | Represents the bounds of /known/ $ArchitectureType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ArchitectureType where
    minBound = ATARM64
    maxBound = ATX86_64

instance Hashable     ArchitectureType
instance NFData       ArchitectureType
instance ToByteString ArchitectureType
instance ToQuery      ArchitectureType
instance ToHeader     ArchitectureType

instance FromXML ArchitectureType where
    parseXML = parseXMLText "ArchitectureType"
