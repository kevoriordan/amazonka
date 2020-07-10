{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.OperatingSystemType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.OperatingSystemType (
  OperatingSystemType (
    ..
    , Linux
    , Windows
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OperatingSystemType = OperatingSystemType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Linux :: OperatingSystemType
pattern Linux = OperatingSystemType' "LINUX"

pattern Windows :: OperatingSystemType
pattern Windows = OperatingSystemType' "WINDOWS"

{-# COMPLETE
  Linux,
  Windows,
  OperatingSystemType' #-}

instance FromText OperatingSystemType where
    parser = (OperatingSystemType' . mk) <$> takeText

instance ToText OperatingSystemType where
    toText (OperatingSystemType' ci) = original ci

-- | Represents an enum of /known/ $OperatingSystemType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OperatingSystemType where
    toEnum i = case i of
        0 -> Linux
        1 -> Windows
        _ -> (error . showText) $ "Unknown index for OperatingSystemType: " <> toText i
    fromEnum x = case x of
        Linux -> 0
        Windows -> 1
        OperatingSystemType' name -> (error . showText) $ "Unknown OperatingSystemType: " <> original name

-- | Represents the bounds of /known/ $OperatingSystemType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OperatingSystemType where
    minBound = Linux
    maxBound = Windows

instance Hashable     OperatingSystemType
instance NFData       OperatingSystemType
instance ToByteString OperatingSystemType
instance ToQuery      OperatingSystemType
instance ToHeader     OperatingSystemType

instance FromJSON OperatingSystemType where
    parseJSON = parseJSONText "OperatingSystemType"
