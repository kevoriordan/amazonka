{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.DocumentStatusType (
  DocumentStatusType (
    ..
    , DSTActive
    , DSTInitialized
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DocumentStatusType = DocumentStatusType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern DSTActive :: DocumentStatusType
pattern DSTActive = DocumentStatusType' "ACTIVE"

pattern DSTInitialized :: DocumentStatusType
pattern DSTInitialized = DocumentStatusType' "INITIALIZED"

{-# COMPLETE
  DSTActive,
  DSTInitialized,
  DocumentStatusType' #-}

instance FromText DocumentStatusType where
    parser = (DocumentStatusType' . mk) <$> takeText

instance ToText DocumentStatusType where
    toText (DocumentStatusType' ci) = original ci

-- | Represents an enum of /known/ $DocumentStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentStatusType where
    toEnum i = case i of
        0 -> DSTActive
        1 -> DSTInitialized
        _ -> (error . showText) $ "Unknown index for DocumentStatusType: " <> toText i
    fromEnum x = case x of
        DSTActive -> 0
        DSTInitialized -> 1
        DocumentStatusType' name -> (error . showText) $ "Unknown DocumentStatusType: " <> original name

-- | Represents the bounds of /known/ $DocumentStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentStatusType where
    minBound = DSTActive
    maxBound = DSTInitialized

instance Hashable     DocumentStatusType
instance NFData       DocumentStatusType
instance ToByteString DocumentStatusType
instance ToQuery      DocumentStatusType
instance ToHeader     DocumentStatusType

instance FromJSON DocumentStatusType where
    parseJSON = parseJSONText "DocumentStatusType"
