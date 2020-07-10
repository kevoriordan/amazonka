{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Cloud9.Types.EnvironmentType (
  EnvironmentType (
    ..
    , EC2
    , SSH
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnvironmentType = EnvironmentType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern EC2 :: EnvironmentType
pattern EC2 = EnvironmentType' "ec2"

pattern SSH :: EnvironmentType
pattern SSH = EnvironmentType' "ssh"

{-# COMPLETE
  EC2,
  SSH,
  EnvironmentType' #-}

instance FromText EnvironmentType where
    parser = (EnvironmentType' . mk) <$> takeText

instance ToText EnvironmentType where
    toText (EnvironmentType' ci) = original ci

-- | Represents an enum of /known/ $EnvironmentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EnvironmentType where
    toEnum i = case i of
        0 -> EC2
        1 -> SSH
        _ -> (error . showText) $ "Unknown index for EnvironmentType: " <> toText i
    fromEnum x = case x of
        EC2 -> 0
        SSH -> 1
        EnvironmentType' name -> (error . showText) $ "Unknown EnvironmentType: " <> original name

-- | Represents the bounds of /known/ $EnvironmentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnvironmentType where
    minBound = EC2
    maxBound = SSH

instance Hashable     EnvironmentType
instance NFData       EnvironmentType
instance ToByteString EnvironmentType
instance ToQuery      EnvironmentType
instance ToHeader     EnvironmentType

instance FromJSON EnvironmentType where
    parseJSON = parseJSONText "EnvironmentType"
