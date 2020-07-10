{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCTenancy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPCTenancy (
  VPCTenancy (
    ..
    , VTDefault
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCTenancy = VPCTenancy' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern VTDefault :: VPCTenancy
pattern VTDefault = VPCTenancy' "default"

{-# COMPLETE
  VTDefault,
  VPCTenancy' #-}

instance FromText VPCTenancy where
    parser = (VPCTenancy' . mk) <$> takeText

instance ToText VPCTenancy where
    toText (VPCTenancy' ci) = original ci

-- | Represents an enum of /known/ $VPCTenancy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPCTenancy where
    toEnum i = case i of
        0 -> VTDefault
        _ -> (error . showText) $ "Unknown index for VPCTenancy: " <> toText i
    fromEnum x = case x of
        VTDefault -> 0
        VPCTenancy' name -> (error . showText) $ "Unknown VPCTenancy: " <> original name

-- | Represents the bounds of /known/ $VPCTenancy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPCTenancy where
    minBound = VTDefault
    maxBound = VTDefault

instance Hashable     VPCTenancy
instance NFData       VPCTenancy
instance ToByteString VPCTenancy
instance ToQuery      VPCTenancy
instance ToHeader     VPCTenancy
