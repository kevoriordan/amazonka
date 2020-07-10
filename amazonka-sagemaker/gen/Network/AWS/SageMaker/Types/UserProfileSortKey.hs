{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserProfileSortKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.UserProfileSortKey (
  UserProfileSortKey (
    ..
    , UPSKCreationTime
    , UPSKLastModifiedTime
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UserProfileSortKey = UserProfileSortKey' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern UPSKCreationTime :: UserProfileSortKey
pattern UPSKCreationTime = UserProfileSortKey' "CreationTime"

pattern UPSKLastModifiedTime :: UserProfileSortKey
pattern UPSKLastModifiedTime = UserProfileSortKey' "LastModifiedTime"

{-# COMPLETE
  UPSKCreationTime,
  UPSKLastModifiedTime,
  UserProfileSortKey' #-}

instance FromText UserProfileSortKey where
    parser = (UserProfileSortKey' . mk) <$> takeText

instance ToText UserProfileSortKey where
    toText (UserProfileSortKey' ci) = original ci

-- | Represents an enum of /known/ $UserProfileSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UserProfileSortKey where
    toEnum i = case i of
        0 -> UPSKCreationTime
        1 -> UPSKLastModifiedTime
        _ -> (error . showText) $ "Unknown index for UserProfileSortKey: " <> toText i
    fromEnum x = case x of
        UPSKCreationTime -> 0
        UPSKLastModifiedTime -> 1
        UserProfileSortKey' name -> (error . showText) $ "Unknown UserProfileSortKey: " <> original name

-- | Represents the bounds of /known/ $UserProfileSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UserProfileSortKey where
    minBound = UPSKCreationTime
    maxBound = UPSKLastModifiedTime

instance Hashable     UserProfileSortKey
instance NFData       UserProfileSortKey
instance ToByteString UserProfileSortKey
instance ToQuery      UserProfileSortKey
instance ToHeader     UserProfileSortKey

instance ToJSON UserProfileSortKey where
    toJSON = toJSONText
