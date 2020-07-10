{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RequestStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.RequestStatus (
  RequestStatus (
    ..
    , Available
    , Creating
    , Failed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RequestStatus = RequestStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Available :: RequestStatus
pattern Available = RequestStatus' "AVAILABLE"

pattern Creating :: RequestStatus
pattern Creating = RequestStatus' "CREATING"

pattern Failed :: RequestStatus
pattern Failed = RequestStatus' "FAILED"

{-# COMPLETE
  Available,
  Creating,
  Failed,
  RequestStatus' #-}

instance FromText RequestStatus where
    parser = (RequestStatus' . mk) <$> takeText

instance ToText RequestStatus where
    toText (RequestStatus' ci) = original ci

-- | Represents an enum of /known/ $RequestStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RequestStatus where
    toEnum i = case i of
        0 -> Available
        1 -> Creating
        2 -> Failed
        _ -> (error . showText) $ "Unknown index for RequestStatus: " <> toText i
    fromEnum x = case x of
        Available -> 0
        Creating -> 1
        Failed -> 2
        RequestStatus' name -> (error . showText) $ "Unknown RequestStatus: " <> original name

-- | Represents the bounds of /known/ $RequestStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RequestStatus where
    minBound = Available
    maxBound = Failed

instance Hashable     RequestStatus
instance NFData       RequestStatus
instance ToByteString RequestStatus
instance ToQuery      RequestStatus
instance ToHeader     RequestStatus

instance FromJSON RequestStatus where
    parseJSON = parseJSONText "RequestStatus"
