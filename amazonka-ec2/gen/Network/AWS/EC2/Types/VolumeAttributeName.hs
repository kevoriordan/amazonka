{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeAttributeName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeAttributeName (
  VolumeAttributeName (
    ..
    , VANAutoEnableIO
    , VANProductCodes
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeAttributeName = VolumeAttributeName' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern VANAutoEnableIO :: VolumeAttributeName
pattern VANAutoEnableIO = VolumeAttributeName' "autoEnableIO"

pattern VANProductCodes :: VolumeAttributeName
pattern VANProductCodes = VolumeAttributeName' "productCodes"

{-# COMPLETE
  VANAutoEnableIO,
  VANProductCodes,
  VolumeAttributeName' #-}

instance FromText VolumeAttributeName where
    parser = (VolumeAttributeName' . mk) <$> takeText

instance ToText VolumeAttributeName where
    toText (VolumeAttributeName' ci) = original ci

-- | Represents an enum of /known/ $VolumeAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VolumeAttributeName where
    toEnum i = case i of
        0 -> VANAutoEnableIO
        1 -> VANProductCodes
        _ -> (error . showText) $ "Unknown index for VolumeAttributeName: " <> toText i
    fromEnum x = case x of
        VANAutoEnableIO -> 0
        VANProductCodes -> 1
        VolumeAttributeName' name -> (error . showText) $ "Unknown VolumeAttributeName: " <> original name

-- | Represents the bounds of /known/ $VolumeAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VolumeAttributeName where
    minBound = VANAutoEnableIO
    maxBound = VANProductCodes

instance Hashable     VolumeAttributeName
instance NFData       VolumeAttributeName
instance ToByteString VolumeAttributeName
instance ToQuery      VolumeAttributeName
instance ToHeader     VolumeAttributeName
