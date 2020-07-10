{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.DataSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.DataSource (
  DataSource (
    ..
    , Agent
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DataSource = DataSource' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Agent :: DataSource
pattern Agent = DataSource' "AGENT"

{-# COMPLETE
  Agent,
  DataSource' #-}

instance FromText DataSource where
    parser = (DataSource' . mk) <$> takeText

instance ToText DataSource where
    toText (DataSource' ci) = original ci

-- | Represents an enum of /known/ $DataSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DataSource where
    toEnum i = case i of
        0 -> Agent
        _ -> (error . showText) $ "Unknown index for DataSource: " <> toText i
    fromEnum x = case x of
        Agent -> 0
        DataSource' name -> (error . showText) $ "Unknown DataSource: " <> original name

-- | Represents the bounds of /known/ $DataSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DataSource where
    minBound = Agent
    maxBound = Agent

instance Hashable     DataSource
instance NFData       DataSource
instance ToByteString DataSource
instance ToQuery      DataSource
instance ToHeader     DataSource

instance FromJSON DataSource where
    parseJSON = parseJSONText "DataSource"
