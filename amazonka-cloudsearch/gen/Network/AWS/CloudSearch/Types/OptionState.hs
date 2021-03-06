{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.OptionState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.OptionState (
  OptionState (
    ..
    , Active
    , FailedToValidate
    , Processing
    , RequiresIndexDocuments
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The state of processing a change to an option. One of:
--
--
--     * RequiresIndexDocuments: The option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.    * Processing: The option's latest value is in the process of being activated.    * Active: The option's latest value is fully deployed.     * FailedToValidate: The option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
--
data OptionState = OptionState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Active :: OptionState
pattern Active = OptionState' "Active"

pattern FailedToValidate :: OptionState
pattern FailedToValidate = OptionState' "FailedToValidate"

pattern Processing :: OptionState
pattern Processing = OptionState' "Processing"

pattern RequiresIndexDocuments :: OptionState
pattern RequiresIndexDocuments = OptionState' "RequiresIndexDocuments"

{-# COMPLETE
  Active,
  FailedToValidate,
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
        1 -> FailedToValidate
        2 -> Processing
        3 -> RequiresIndexDocuments
        _ -> (error . showText) $ "Unknown index for OptionState: " <> toText i
    fromEnum x = case x of
        Active -> 0
        FailedToValidate -> 1
        Processing -> 2
        RequiresIndexDocuments -> 3
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

instance FromXML OptionState where
    parseXML = parseXMLText "OptionState"
