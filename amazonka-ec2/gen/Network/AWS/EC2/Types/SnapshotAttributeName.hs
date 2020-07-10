{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotAttributeName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SnapshotAttributeName (
  SnapshotAttributeName (
    ..
    , SANCreateVolumePermission
    , SANProductCodes
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SnapshotAttributeName = SnapshotAttributeName' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern SANCreateVolumePermission :: SnapshotAttributeName
pattern SANCreateVolumePermission = SnapshotAttributeName' "createVolumePermission"

pattern SANProductCodes :: SnapshotAttributeName
pattern SANProductCodes = SnapshotAttributeName' "productCodes"

{-# COMPLETE
  SANCreateVolumePermission,
  SANProductCodes,
  SnapshotAttributeName' #-}

instance FromText SnapshotAttributeName where
    parser = (SnapshotAttributeName' . mk) <$> takeText

instance ToText SnapshotAttributeName where
    toText (SnapshotAttributeName' ci) = original ci

-- | Represents an enum of /known/ $SnapshotAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SnapshotAttributeName where
    toEnum i = case i of
        0 -> SANCreateVolumePermission
        1 -> SANProductCodes
        _ -> (error . showText) $ "Unknown index for SnapshotAttributeName: " <> toText i
    fromEnum x = case x of
        SANCreateVolumePermission -> 0
        SANProductCodes -> 1
        SnapshotAttributeName' name -> (error . showText) $ "Unknown SnapshotAttributeName: " <> original name

-- | Represents the bounds of /known/ $SnapshotAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SnapshotAttributeName where
    minBound = SANCreateVolumePermission
    maxBound = SANProductCodes

instance Hashable     SnapshotAttributeName
instance NFData       SnapshotAttributeName
instance ToByteString SnapshotAttributeName
instance ToQuery      SnapshotAttributeName
instance ToHeader     SnapshotAttributeName
