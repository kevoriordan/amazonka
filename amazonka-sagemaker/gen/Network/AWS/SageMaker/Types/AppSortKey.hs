{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppSortKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AppSortKey (
  AppSortKey (
    ..
    , ASKCreationTime
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AppSortKey = AppSortKey' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ASKCreationTime :: AppSortKey
pattern ASKCreationTime = AppSortKey' "CreationTime"

{-# COMPLETE
  ASKCreationTime,
  AppSortKey' #-}

instance FromText AppSortKey where
    parser = (AppSortKey' . mk) <$> takeText

instance ToText AppSortKey where
    toText (AppSortKey' ci) = original ci

-- | Represents an enum of /known/ $AppSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AppSortKey where
    toEnum i = case i of
        0 -> ASKCreationTime
        _ -> (error . showText) $ "Unknown index for AppSortKey: " <> toText i
    fromEnum x = case x of
        ASKCreationTime -> 0
        AppSortKey' name -> (error . showText) $ "Unknown AppSortKey: " <> original name

-- | Represents the bounds of /known/ $AppSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AppSortKey where
    minBound = ASKCreationTime
    maxBound = ASKCreationTime

instance Hashable     AppSortKey
instance NFData       AppSortKey
instance ToByteString AppSortKey
instance ToQuery      AppSortKey
instance ToHeader     AppSortKey

instance ToJSON AppSortKey where
    toJSON = toJSONText
