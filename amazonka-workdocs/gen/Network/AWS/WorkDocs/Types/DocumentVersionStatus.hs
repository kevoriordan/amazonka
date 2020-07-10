{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentVersionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.DocumentVersionStatus (
  DocumentVersionStatus (
    ..
    , DVSActive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DocumentVersionStatus = DocumentVersionStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern DVSActive :: DocumentVersionStatus
pattern DVSActive = DocumentVersionStatus' "ACTIVE"

{-# COMPLETE
  DVSActive,
  DocumentVersionStatus' #-}

instance FromText DocumentVersionStatus where
    parser = (DocumentVersionStatus' . mk) <$> takeText

instance ToText DocumentVersionStatus where
    toText (DocumentVersionStatus' ci) = original ci

-- | Represents an enum of /known/ $DocumentVersionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentVersionStatus where
    toEnum i = case i of
        0 -> DVSActive
        _ -> (error . showText) $ "Unknown index for DocumentVersionStatus: " <> toText i
    fromEnum x = case x of
        DVSActive -> 0
        DocumentVersionStatus' name -> (error . showText) $ "Unknown DocumentVersionStatus: " <> original name

-- | Represents the bounds of /known/ $DocumentVersionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentVersionStatus where
    minBound = DVSActive
    maxBound = DVSActive

instance Hashable     DocumentVersionStatus
instance NFData       DocumentVersionStatus
instance ToByteString DocumentVersionStatus
instance ToQuery      DocumentVersionStatus
instance ToHeader     DocumentVersionStatus

instance ToJSON DocumentVersionStatus where
    toJSON = toJSONText
