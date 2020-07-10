{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetActionType (
  DatasetActionType (
    ..
    , Container
    , Query
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DatasetActionType = DatasetActionType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Container :: DatasetActionType
pattern Container = DatasetActionType' "CONTAINER"

pattern Query :: DatasetActionType
pattern Query = DatasetActionType' "QUERY"

{-# COMPLETE
  Container,
  Query,
  DatasetActionType' #-}

instance FromText DatasetActionType where
    parser = (DatasetActionType' . mk) <$> takeText

instance ToText DatasetActionType where
    toText (DatasetActionType' ci) = original ci

-- | Represents an enum of /known/ $DatasetActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DatasetActionType where
    toEnum i = case i of
        0 -> Container
        1 -> Query
        _ -> (error . showText) $ "Unknown index for DatasetActionType: " <> toText i
    fromEnum x = case x of
        Container -> 0
        Query -> 1
        DatasetActionType' name -> (error . showText) $ "Unknown DatasetActionType: " <> original name

-- | Represents the bounds of /known/ $DatasetActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DatasetActionType where
    minBound = Container
    maxBound = Query

instance Hashable     DatasetActionType
instance NFData       DatasetActionType
instance ToByteString DatasetActionType
instance ToQuery      DatasetActionType
instance ToHeader     DatasetActionType

instance FromJSON DatasetActionType where
    parseJSON = parseJSONText "DatasetActionType"
