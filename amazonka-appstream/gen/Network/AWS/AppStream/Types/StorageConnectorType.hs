{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StorageConnectorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.StorageConnectorType (
  StorageConnectorType (
    ..
    , GoogleDrive
    , Homefolders
    , OneDrive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The type of storage connector.
--
--
data StorageConnectorType = StorageConnectorType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern GoogleDrive :: StorageConnectorType
pattern GoogleDrive = StorageConnectorType' "GOOGLE_DRIVE"

pattern Homefolders :: StorageConnectorType
pattern Homefolders = StorageConnectorType' "HOMEFOLDERS"

pattern OneDrive :: StorageConnectorType
pattern OneDrive = StorageConnectorType' "ONE_DRIVE"

{-# COMPLETE
  GoogleDrive,
  Homefolders,
  OneDrive,
  StorageConnectorType' #-}

instance FromText StorageConnectorType where
    parser = (StorageConnectorType' . mk) <$> takeText

instance ToText StorageConnectorType where
    toText (StorageConnectorType' ci) = original ci

-- | Represents an enum of /known/ $StorageConnectorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StorageConnectorType where
    toEnum i = case i of
        0 -> GoogleDrive
        1 -> Homefolders
        2 -> OneDrive
        _ -> (error . showText) $ "Unknown index for StorageConnectorType: " <> toText i
    fromEnum x = case x of
        GoogleDrive -> 0
        Homefolders -> 1
        OneDrive -> 2
        StorageConnectorType' name -> (error . showText) $ "Unknown StorageConnectorType: " <> original name

-- | Represents the bounds of /known/ $StorageConnectorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StorageConnectorType where
    minBound = GoogleDrive
    maxBound = OneDrive

instance Hashable     StorageConnectorType
instance NFData       StorageConnectorType
instance ToByteString StorageConnectorType
instance ToQuery      StorageConnectorType
instance ToHeader     StorageConnectorType

instance ToJSON StorageConnectorType where
    toJSON = toJSONText

instance FromJSON StorageConnectorType where
    parseJSON = parseJSONText "StorageConnectorType"
