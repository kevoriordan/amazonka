{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogEncryptionMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CatalogEncryptionMode (
  CatalogEncryptionMode (
    ..
    , CEMDisabled
    , CEMSseKMS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CatalogEncryptionMode = CatalogEncryptionMode' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern CEMDisabled :: CatalogEncryptionMode
pattern CEMDisabled = CatalogEncryptionMode' "DISABLED"

pattern CEMSseKMS :: CatalogEncryptionMode
pattern CEMSseKMS = CatalogEncryptionMode' "SSE-KMS"

{-# COMPLETE
  CEMDisabled,
  CEMSseKMS,
  CatalogEncryptionMode' #-}

instance FromText CatalogEncryptionMode where
    parser = (CatalogEncryptionMode' . mk) <$> takeText

instance ToText CatalogEncryptionMode where
    toText (CatalogEncryptionMode' ci) = original ci

-- | Represents an enum of /known/ $CatalogEncryptionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CatalogEncryptionMode where
    toEnum i = case i of
        0 -> CEMDisabled
        1 -> CEMSseKMS
        _ -> (error . showText) $ "Unknown index for CatalogEncryptionMode: " <> toText i
    fromEnum x = case x of
        CEMDisabled -> 0
        CEMSseKMS -> 1
        CatalogEncryptionMode' name -> (error . showText) $ "Unknown CatalogEncryptionMode: " <> original name

-- | Represents the bounds of /known/ $CatalogEncryptionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CatalogEncryptionMode where
    minBound = CEMDisabled
    maxBound = CEMSseKMS

instance Hashable     CatalogEncryptionMode
instance NFData       CatalogEncryptionMode
instance ToByteString CatalogEncryptionMode
instance ToQuery      CatalogEncryptionMode
instance ToHeader     CatalogEncryptionMode

instance ToJSON CatalogEncryptionMode where
    toJSON = toJSONText

instance FromJSON CatalogEncryptionMode where
    parseJSON = parseJSONText "CatalogEncryptionMode"
