{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ListWorkteamsSortByOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ListWorkteamsSortByOptions (
  ListWorkteamsSortByOptions (
    ..
    , LWSBOCreateDate
    , LWSBOName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ListWorkteamsSortByOptions = ListWorkteamsSortByOptions' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern LWSBOCreateDate :: ListWorkteamsSortByOptions
pattern LWSBOCreateDate = ListWorkteamsSortByOptions' "CreateDate"

pattern LWSBOName :: ListWorkteamsSortByOptions
pattern LWSBOName = ListWorkteamsSortByOptions' "Name"

{-# COMPLETE
  LWSBOCreateDate,
  LWSBOName,
  ListWorkteamsSortByOptions' #-}

instance FromText ListWorkteamsSortByOptions where
    parser = (ListWorkteamsSortByOptions' . mk) <$> takeText

instance ToText ListWorkteamsSortByOptions where
    toText (ListWorkteamsSortByOptions' ci) = original ci

-- | Represents an enum of /known/ $ListWorkteamsSortByOptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ListWorkteamsSortByOptions where
    toEnum i = case i of
        0 -> LWSBOCreateDate
        1 -> LWSBOName
        _ -> (error . showText) $ "Unknown index for ListWorkteamsSortByOptions: " <> toText i
    fromEnum x = case x of
        LWSBOCreateDate -> 0
        LWSBOName -> 1
        ListWorkteamsSortByOptions' name -> (error . showText) $ "Unknown ListWorkteamsSortByOptions: " <> original name

-- | Represents the bounds of /known/ $ListWorkteamsSortByOptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ListWorkteamsSortByOptions where
    minBound = LWSBOCreateDate
    maxBound = LWSBOName

instance Hashable     ListWorkteamsSortByOptions
instance NFData       ListWorkteamsSortByOptions
instance ToByteString ListWorkteamsSortByOptions
instance ToQuery      ListWorkteamsSortByOptions
instance ToHeader     ListWorkteamsSortByOptions

instance ToJSON ListWorkteamsSortByOptions where
    toJSON = toJSONText
