{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MulticastSupportValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.MulticastSupportValue (
  MulticastSupportValue (
    ..
    , MSVDisable
    , MSVEnable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data MulticastSupportValue = MulticastSupportValue' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern MSVDisable :: MulticastSupportValue
pattern MSVDisable = MulticastSupportValue' "disable"

pattern MSVEnable :: MulticastSupportValue
pattern MSVEnable = MulticastSupportValue' "enable"

{-# COMPLETE
  MSVDisable,
  MSVEnable,
  MulticastSupportValue' #-}

instance FromText MulticastSupportValue where
    parser = (MulticastSupportValue' . mk) <$> takeText

instance ToText MulticastSupportValue where
    toText (MulticastSupportValue' ci) = original ci

-- | Represents an enum of /known/ $MulticastSupportValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MulticastSupportValue where
    toEnum i = case i of
        0 -> MSVDisable
        1 -> MSVEnable
        _ -> (error . showText) $ "Unknown index for MulticastSupportValue: " <> toText i
    fromEnum x = case x of
        MSVDisable -> 0
        MSVEnable -> 1
        MulticastSupportValue' name -> (error . showText) $ "Unknown MulticastSupportValue: " <> original name

-- | Represents the bounds of /known/ $MulticastSupportValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MulticastSupportValue where
    minBound = MSVDisable
    maxBound = MSVEnable

instance Hashable     MulticastSupportValue
instance NFData       MulticastSupportValue
instance ToByteString MulticastSupportValue
instance ToQuery      MulticastSupportValue
instance ToHeader     MulticastSupportValue

instance FromXML MulticastSupportValue where
    parseXML = parseXMLText "MulticastSupportValue"
