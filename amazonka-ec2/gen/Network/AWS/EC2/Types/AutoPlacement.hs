{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AutoPlacement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AutoPlacement (
  AutoPlacement (
    ..
    , APON
    , APOff
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AutoPlacement = AutoPlacement' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern APON :: AutoPlacement
pattern APON = AutoPlacement' "on"

pattern APOff :: AutoPlacement
pattern APOff = AutoPlacement' "off"

{-# COMPLETE
  APON,
  APOff,
  AutoPlacement' #-}

instance FromText AutoPlacement where
    parser = (AutoPlacement' . mk) <$> takeText

instance ToText AutoPlacement where
    toText (AutoPlacement' ci) = original ci

-- | Represents an enum of /known/ $AutoPlacement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoPlacement where
    toEnum i = case i of
        0 -> APON
        1 -> APOff
        _ -> (error . showText) $ "Unknown index for AutoPlacement: " <> toText i
    fromEnum x = case x of
        APON -> 0
        APOff -> 1
        AutoPlacement' name -> (error . showText) $ "Unknown AutoPlacement: " <> original name

-- | Represents the bounds of /known/ $AutoPlacement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoPlacement where
    minBound = APON
    maxBound = APOff

instance Hashable     AutoPlacement
instance NFData       AutoPlacement
instance ToByteString AutoPlacement
instance ToQuery      AutoPlacement
instance ToHeader     AutoPlacement

instance FromXML AutoPlacement where
    parseXML = parseXMLText "AutoPlacement"
