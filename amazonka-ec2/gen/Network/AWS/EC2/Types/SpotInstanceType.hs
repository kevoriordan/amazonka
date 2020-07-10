{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotInstanceType (
  SpotInstanceType (
    ..
    , OneTime
    , Persistent
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SpotInstanceType = SpotInstanceType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern OneTime :: SpotInstanceType
pattern OneTime = SpotInstanceType' "one-time"

pattern Persistent :: SpotInstanceType
pattern Persistent = SpotInstanceType' "persistent"

{-# COMPLETE
  OneTime,
  Persistent,
  SpotInstanceType' #-}

instance FromText SpotInstanceType where
    parser = (SpotInstanceType' . mk) <$> takeText

instance ToText SpotInstanceType where
    toText (SpotInstanceType' ci) = original ci

-- | Represents an enum of /known/ $SpotInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SpotInstanceType where
    toEnum i = case i of
        0 -> OneTime
        1 -> Persistent
        _ -> (error . showText) $ "Unknown index for SpotInstanceType: " <> toText i
    fromEnum x = case x of
        OneTime -> 0
        Persistent -> 1
        SpotInstanceType' name -> (error . showText) $ "Unknown SpotInstanceType: " <> original name

-- | Represents the bounds of /known/ $SpotInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SpotInstanceType where
    minBound = OneTime
    maxBound = Persistent

instance Hashable     SpotInstanceType
instance NFData       SpotInstanceType
instance ToByteString SpotInstanceType
instance ToQuery      SpotInstanceType
instance ToHeader     SpotInstanceType

instance FromXML SpotInstanceType where
    parseXML = parseXMLText "SpotInstanceType"
