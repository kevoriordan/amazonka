{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AllowsMultipleInstanceTypes (
  AllowsMultipleInstanceTypes (
    ..
    , ON
    , Off
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AllowsMultipleInstanceTypes = AllowsMultipleInstanceTypes' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern ON :: AllowsMultipleInstanceTypes
pattern ON = AllowsMultipleInstanceTypes' "on"

pattern Off :: AllowsMultipleInstanceTypes
pattern Off = AllowsMultipleInstanceTypes' "off"

{-# COMPLETE
  ON,
  Off,
  AllowsMultipleInstanceTypes' #-}

instance FromText AllowsMultipleInstanceTypes where
    parser = (AllowsMultipleInstanceTypes' . mk) <$> takeText

instance ToText AllowsMultipleInstanceTypes where
    toText (AllowsMultipleInstanceTypes' ci) = original ci

-- | Represents an enum of /known/ $AllowsMultipleInstanceTypes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AllowsMultipleInstanceTypes where
    toEnum i = case i of
        0 -> ON
        1 -> Off
        _ -> (error . showText) $ "Unknown index for AllowsMultipleInstanceTypes: " <> toText i
    fromEnum x = case x of
        ON -> 0
        Off -> 1
        AllowsMultipleInstanceTypes' name -> (error . showText) $ "Unknown AllowsMultipleInstanceTypes: " <> original name

-- | Represents the bounds of /known/ $AllowsMultipleInstanceTypes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AllowsMultipleInstanceTypes where
    minBound = ON
    maxBound = Off

instance Hashable     AllowsMultipleInstanceTypes
instance NFData       AllowsMultipleInstanceTypes
instance ToByteString AllowsMultipleInstanceTypes
instance ToQuery      AllowsMultipleInstanceTypes
instance ToHeader     AllowsMultipleInstanceTypes

instance FromXML AllowsMultipleInstanceTypes where
    parseXML = parseXMLText "AllowsMultipleInstanceTypes"
