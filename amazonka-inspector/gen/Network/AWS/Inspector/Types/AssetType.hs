{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssetType (
  AssetType (
    ..
    , EC2Instance
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssetType = AssetType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern EC2Instance :: AssetType
pattern EC2Instance = AssetType' "ec2-instance"

{-# COMPLETE
  EC2Instance,
  AssetType' #-}

instance FromText AssetType where
    parser = (AssetType' . mk) <$> takeText

instance ToText AssetType where
    toText (AssetType' ci) = original ci

-- | Represents an enum of /known/ $AssetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssetType where
    toEnum i = case i of
        0 -> EC2Instance
        _ -> (error . showText) $ "Unknown index for AssetType: " <> toText i
    fromEnum x = case x of
        EC2Instance -> 0
        AssetType' name -> (error . showText) $ "Unknown AssetType: " <> original name

-- | Represents the bounds of /known/ $AssetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssetType where
    minBound = EC2Instance
    maxBound = EC2Instance

instance Hashable     AssetType
instance NFData       AssetType
instance ToByteString AssetType
instance ToQuery      AssetType
instance ToHeader     AssetType

instance FromJSON AssetType where
    parseJSON = parseJSONText "AssetType"
