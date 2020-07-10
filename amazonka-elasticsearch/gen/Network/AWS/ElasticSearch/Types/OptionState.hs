{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OptionState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.OptionState (
  OptionState (
    ..
    , Active
    , Processing
    , RequiresIndexDocuments
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The state of a requested change. One of the following:
--
--
--     * Processing: The request change is still in-process.    * Active: The request change is processed and deployed to the Elasticsearch domain.
--
data OptionState = OptionState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Active :: OptionState
pattern Active = OptionState' "Active"

pattern Processing :: OptionState
pattern Processing = OptionState' "Processing"

pattern RequiresIndexDocuments :: OptionState
pattern RequiresIndexDocuments = OptionState' "RequiresIndexDocuments"

{-# COMPLETE
  Active,
  Processing,
  RequiresIndexDocuments,
  OptionState' #-}

instance FromText OptionState where
    parser = (OptionState' . mk) <$> takeText

instance ToText OptionState where
    toText (OptionState' ci) = original ci

-- | Represents an enum of /known/ $OptionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OptionState where
    toEnum i = case i of
        0 -> Active
        1 -> Processing
        2 -> RequiresIndexDocuments
        _ -> (error . showText) $ "Unknown index for OptionState: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Processing -> 1
        RequiresIndexDocuments -> 2
        OptionState' name -> (error . showText) $ "Unknown OptionState: " <> original name

-- | Represents the bounds of /known/ $OptionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OptionState where
    minBound = Active
    maxBound = RequiresIndexDocuments

instance Hashable     OptionState
instance NFData       OptionState
instance ToByteString OptionState
instance ToQuery      OptionState
instance ToHeader     OptionState

instance FromJSON OptionState where
    parseJSON = parseJSONText "OptionState"
