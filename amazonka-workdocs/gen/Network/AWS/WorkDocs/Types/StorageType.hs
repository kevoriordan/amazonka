{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.StorageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.StorageType (
  StorageType (
    ..
    , Quota
    , Unlimited
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StorageType = StorageType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Quota :: StorageType
pattern Quota = StorageType' "QUOTA"

pattern Unlimited :: StorageType
pattern Unlimited = StorageType' "UNLIMITED"

{-# COMPLETE
  Quota,
  Unlimited,
  StorageType' #-}

instance FromText StorageType where
    parser = (StorageType' . mk) <$> takeText

instance ToText StorageType where
    toText (StorageType' ci) = original ci

-- | Represents an enum of /known/ $StorageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StorageType where
    toEnum i = case i of
        0 -> Quota
        1 -> Unlimited
        _ -> (error . showText) $ "Unknown index for StorageType: " <> toText i
    fromEnum x = case x of
        Quota -> 0
        Unlimited -> 1
        StorageType' name -> (error . showText) $ "Unknown StorageType: " <> original name

-- | Represents the bounds of /known/ $StorageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StorageType where
    minBound = Quota
    maxBound = Unlimited

instance Hashable     StorageType
instance NFData       StorageType
instance ToByteString StorageType
instance ToQuery      StorageType
instance ToHeader     StorageType

instance ToJSON StorageType where
    toJSON = toJSONText

instance FromJSON StorageType where
    parseJSON = parseJSONText "StorageType"
