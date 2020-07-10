{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentStatus (
  DocumentStatus (
    ..
    , DSActive
    , DSCreating
    , DSDeleting
    , DSFailed
    , DSUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The status of a document.
--
--
data DocumentStatus = DocumentStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern DSActive :: DocumentStatus
pattern DSActive = DocumentStatus' "Active"

pattern DSCreating :: DocumentStatus
pattern DSCreating = DocumentStatus' "Creating"

pattern DSDeleting :: DocumentStatus
pattern DSDeleting = DocumentStatus' "Deleting"

pattern DSFailed :: DocumentStatus
pattern DSFailed = DocumentStatus' "Failed"

pattern DSUpdating :: DocumentStatus
pattern DSUpdating = DocumentStatus' "Updating"

{-# COMPLETE
  DSActive,
  DSCreating,
  DSDeleting,
  DSFailed,
  DSUpdating,
  DocumentStatus' #-}

instance FromText DocumentStatus where
    parser = (DocumentStatus' . mk) <$> takeText

instance ToText DocumentStatus where
    toText (DocumentStatus' ci) = original ci

-- | Represents an enum of /known/ $DocumentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentStatus where
    toEnum i = case i of
        0 -> DSActive
        1 -> DSCreating
        2 -> DSDeleting
        3 -> DSFailed
        4 -> DSUpdating
        _ -> (error . showText) $ "Unknown index for DocumentStatus: " <> toText i
    fromEnum x = case x of
        DSActive -> 0
        DSCreating -> 1
        DSDeleting -> 2
        DSFailed -> 3
        DSUpdating -> 4
        DocumentStatus' name -> (error . showText) $ "Unknown DocumentStatus: " <> original name

-- | Represents the bounds of /known/ $DocumentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentStatus where
    minBound = DSActive
    maxBound = DSUpdating

instance Hashable     DocumentStatus
instance NFData       DocumentStatus
instance ToByteString DocumentStatus
instance ToQuery      DocumentStatus
instance ToHeader     DocumentStatus

instance FromJSON DocumentStatus where
    parseJSON = parseJSONText "DocumentStatus"
