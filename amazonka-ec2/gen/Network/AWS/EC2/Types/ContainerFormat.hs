{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ContainerFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ContainerFormat (
  ContainerFormat (
    ..
    , Ova
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ContainerFormat = ContainerFormat' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Ova :: ContainerFormat
pattern Ova = ContainerFormat' "ova"

{-# COMPLETE
  Ova,
  ContainerFormat' #-}

instance FromText ContainerFormat where
    parser = (ContainerFormat' . mk) <$> takeText

instance ToText ContainerFormat where
    toText (ContainerFormat' ci) = original ci

-- | Represents an enum of /known/ $ContainerFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContainerFormat where
    toEnum i = case i of
        0 -> Ova
        _ -> (error . showText) $ "Unknown index for ContainerFormat: " <> toText i
    fromEnum x = case x of
        Ova -> 0
        ContainerFormat' name -> (error . showText) $ "Unknown ContainerFormat: " <> original name

-- | Represents the bounds of /known/ $ContainerFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContainerFormat where
    minBound = Ova
    maxBound = Ova

instance Hashable     ContainerFormat
instance NFData       ContainerFormat
instance ToByteString ContainerFormat
instance ToQuery      ContainerFormat
instance ToHeader     ContainerFormat

instance FromXML ContainerFormat where
    parseXML = parseXMLText "ContainerFormat"
