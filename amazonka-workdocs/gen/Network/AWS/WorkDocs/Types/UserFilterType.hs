{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserFilterType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.UserFilterType (
  UserFilterType (
    ..
    , ActivePending
    , All
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UserFilterType = UserFilterType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ActivePending :: UserFilterType
pattern ActivePending = UserFilterType' "ACTIVE_PENDING"

pattern All :: UserFilterType
pattern All = UserFilterType' "ALL"

{-# COMPLETE
  ActivePending,
  All,
  UserFilterType' #-}

instance FromText UserFilterType where
    parser = (UserFilterType' . mk) <$> takeText

instance ToText UserFilterType where
    toText (UserFilterType' ci) = original ci

-- | Represents an enum of /known/ $UserFilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UserFilterType where
    toEnum i = case i of
        0 -> ActivePending
        1 -> All
        _ -> (error . showText) $ "Unknown index for UserFilterType: " <> toText i
    fromEnum x = case x of
        ActivePending -> 0
        All -> 1
        UserFilterType' name -> (error . showText) $ "Unknown UserFilterType: " <> original name

-- | Represents the bounds of /known/ $UserFilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UserFilterType where
    minBound = ActivePending
    maxBound = All

instance Hashable     UserFilterType
instance NFData       UserFilterType
instance ToByteString UserFilterType
instance ToQuery      UserFilterType
instance ToHeader     UserFilterType

instance ToJSON UserFilterType where
    toJSON = toJSONText
