{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.VMManagerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.VMManagerType (
  VMManagerType (
    ..
    , HypervManager
    , Scvmm
    , Vsphere
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data VMManagerType = VMManagerType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern HypervManager :: VMManagerType
pattern HypervManager = VMManagerType' "HYPERV-MANAGER"

pattern Scvmm :: VMManagerType
pattern Scvmm = VMManagerType' "SCVMM"

pattern Vsphere :: VMManagerType
pattern Vsphere = VMManagerType' "VSPHERE"

{-# COMPLETE
  HypervManager,
  Scvmm,
  Vsphere,
  VMManagerType' #-}

instance FromText VMManagerType where
    parser = (VMManagerType' . mk) <$> takeText

instance ToText VMManagerType where
    toText (VMManagerType' ci) = original ci

-- | Represents an enum of /known/ $VMManagerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VMManagerType where
    toEnum i = case i of
        0 -> HypervManager
        1 -> Scvmm
        2 -> Vsphere
        _ -> (error . showText) $ "Unknown index for VMManagerType: " <> toText i
    fromEnum x = case x of
        HypervManager -> 0
        Scvmm -> 1
        Vsphere -> 2
        VMManagerType' name -> (error . showText) $ "Unknown VMManagerType: " <> original name

-- | Represents the bounds of /known/ $VMManagerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VMManagerType where
    minBound = HypervManager
    maxBound = Vsphere

instance Hashable     VMManagerType
instance NFData       VMManagerType
instance ToByteString VMManagerType
instance ToQuery      VMManagerType
instance ToHeader     VMManagerType

instance ToJSON VMManagerType where
    toJSON = toJSONText

instance FromJSON VMManagerType where
    parseJSON = parseJSONText "VMManagerType"
