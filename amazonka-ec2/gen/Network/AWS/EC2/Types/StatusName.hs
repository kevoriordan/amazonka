{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StatusName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.StatusName (
  StatusName (
    ..
    , Reachability
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data StatusName = StatusName' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Reachability :: StatusName
pattern Reachability = StatusName' "reachability"

{-# COMPLETE
  Reachability,
  StatusName' #-}

instance FromText StatusName where
    parser = (StatusName' . mk) <$> takeText

instance ToText StatusName where
    toText (StatusName' ci) = original ci

-- | Represents an enum of /known/ $StatusName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StatusName where
    toEnum i = case i of
        0 -> Reachability
        _ -> (error . showText) $ "Unknown index for StatusName: " <> toText i
    fromEnum x = case x of
        Reachability -> 0
        StatusName' name -> (error . showText) $ "Unknown StatusName: " <> original name

-- | Represents the bounds of /known/ $StatusName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StatusName where
    minBound = Reachability
    maxBound = Reachability

instance Hashable     StatusName
instance NFData       StatusName
instance ToByteString StatusName
instance ToQuery      StatusName
instance ToHeader     StatusName

instance FromXML StatusName where
    parseXML = parseXMLText "StatusName"
