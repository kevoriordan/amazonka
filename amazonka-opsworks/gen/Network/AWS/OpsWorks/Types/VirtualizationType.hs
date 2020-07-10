{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.VirtualizationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.VirtualizationType (
  VirtualizationType (
    ..
    , HVM
    , Paravirtual
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data VirtualizationType = VirtualizationType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern HVM :: VirtualizationType
pattern HVM = VirtualizationType' "hvm"

pattern Paravirtual :: VirtualizationType
pattern Paravirtual = VirtualizationType' "paravirtual"

{-# COMPLETE
  HVM,
  Paravirtual,
  VirtualizationType' #-}

instance FromText VirtualizationType where
    parser = (VirtualizationType' . mk) <$> takeText

instance ToText VirtualizationType where
    toText (VirtualizationType' ci) = original ci

-- | Represents an enum of /known/ $VirtualizationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VirtualizationType where
    toEnum i = case i of
        0 -> HVM
        1 -> Paravirtual
        _ -> (error . showText) $ "Unknown index for VirtualizationType: " <> toText i
    fromEnum x = case x of
        HVM -> 0
        Paravirtual -> 1
        VirtualizationType' name -> (error . showText) $ "Unknown VirtualizationType: " <> original name

-- | Represents the bounds of /known/ $VirtualizationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VirtualizationType where
    minBound = HVM
    maxBound = Paravirtual

instance Hashable     VirtualizationType
instance NFData       VirtualizationType
instance ToByteString VirtualizationType
instance ToQuery      VirtualizationType
instance ToHeader     VirtualizationType

instance FromJSON VirtualizationType where
    parseJSON = parseJSONText "VirtualizationType"
