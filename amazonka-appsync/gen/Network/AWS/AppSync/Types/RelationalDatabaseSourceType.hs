{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.RelationalDatabaseSourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.RelationalDatabaseSourceType (
  RelationalDatabaseSourceType (
    ..
    , RDSHTTPEndpoint
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RelationalDatabaseSourceType = RelationalDatabaseSourceType' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern RDSHTTPEndpoint :: RelationalDatabaseSourceType
pattern RDSHTTPEndpoint = RelationalDatabaseSourceType' "RDS_HTTP_ENDPOINT"

{-# COMPLETE
  RDSHTTPEndpoint,
  RelationalDatabaseSourceType' #-}

instance FromText RelationalDatabaseSourceType where
    parser = (RelationalDatabaseSourceType' . mk) <$> takeText

instance ToText RelationalDatabaseSourceType where
    toText (RelationalDatabaseSourceType' ci) = original ci

-- | Represents an enum of /known/ $RelationalDatabaseSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RelationalDatabaseSourceType where
    toEnum i = case i of
        0 -> RDSHTTPEndpoint
        _ -> (error . showText) $ "Unknown index for RelationalDatabaseSourceType: " <> toText i
    fromEnum x = case x of
        RDSHTTPEndpoint -> 0
        RelationalDatabaseSourceType' name -> (error . showText) $ "Unknown RelationalDatabaseSourceType: " <> original name

-- | Represents the bounds of /known/ $RelationalDatabaseSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RelationalDatabaseSourceType where
    minBound = RDSHTTPEndpoint
    maxBound = RDSHTTPEndpoint

instance Hashable     RelationalDatabaseSourceType
instance NFData       RelationalDatabaseSourceType
instance ToByteString RelationalDatabaseSourceType
instance ToQuery      RelationalDatabaseSourceType
instance ToHeader     RelationalDatabaseSourceType

instance ToJSON RelationalDatabaseSourceType where
    toJSON = toJSONText

instance FromJSON RelationalDatabaseSourceType where
    parseJSON = parseJSONText "RelationalDatabaseSourceType"
