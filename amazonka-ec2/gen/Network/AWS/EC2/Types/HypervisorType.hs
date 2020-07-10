{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HypervisorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.HypervisorType (
  HypervisorType (
    ..
    , HTOvm
    , HTXen
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data HypervisorType = HypervisorType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern HTOvm :: HypervisorType
pattern HTOvm = HypervisorType' "ovm"

pattern HTXen :: HypervisorType
pattern HTXen = HypervisorType' "xen"

{-# COMPLETE
  HTOvm,
  HTXen,
  HypervisorType' #-}

instance FromText HypervisorType where
    parser = (HypervisorType' . mk) <$> takeText

instance ToText HypervisorType where
    toText (HypervisorType' ci) = original ci

-- | Represents an enum of /known/ $HypervisorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HypervisorType where
    toEnum i = case i of
        0 -> HTOvm
        1 -> HTXen
        _ -> (error . showText) $ "Unknown index for HypervisorType: " <> toText i
    fromEnum x = case x of
        HTOvm -> 0
        HTXen -> 1
        HypervisorType' name -> (error . showText) $ "Unknown HypervisorType: " <> original name

-- | Represents the bounds of /known/ $HypervisorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HypervisorType where
    minBound = HTOvm
    maxBound = HTXen

instance Hashable     HypervisorType
instance NFData       HypervisorType
instance ToByteString HypervisorType
instance ToQuery      HypervisorType
instance ToHeader     HypervisorType

instance FromXML HypervisorType where
    parseXML = parseXMLText "HypervisorType"
