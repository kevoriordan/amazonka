{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ApplicationRevisionSortBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.ApplicationRevisionSortBy (
  ApplicationRevisionSortBy (
    ..
    , FirstUsedTime
    , LastUsedTime
    , RegisterTime
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ApplicationRevisionSortBy = ApplicationRevisionSortBy' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern FirstUsedTime :: ApplicationRevisionSortBy
pattern FirstUsedTime = ApplicationRevisionSortBy' "firstUsedTime"

pattern LastUsedTime :: ApplicationRevisionSortBy
pattern LastUsedTime = ApplicationRevisionSortBy' "lastUsedTime"

pattern RegisterTime :: ApplicationRevisionSortBy
pattern RegisterTime = ApplicationRevisionSortBy' "registerTime"

{-# COMPLETE
  FirstUsedTime,
  LastUsedTime,
  RegisterTime,
  ApplicationRevisionSortBy' #-}

instance FromText ApplicationRevisionSortBy where
    parser = (ApplicationRevisionSortBy' . mk) <$> takeText

instance ToText ApplicationRevisionSortBy where
    toText (ApplicationRevisionSortBy' ci) = original ci

-- | Represents an enum of /known/ $ApplicationRevisionSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ApplicationRevisionSortBy where
    toEnum i = case i of
        0 -> FirstUsedTime
        1 -> LastUsedTime
        2 -> RegisterTime
        _ -> (error . showText) $ "Unknown index for ApplicationRevisionSortBy: " <> toText i
    fromEnum x = case x of
        FirstUsedTime -> 0
        LastUsedTime -> 1
        RegisterTime -> 2
        ApplicationRevisionSortBy' name -> (error . showText) $ "Unknown ApplicationRevisionSortBy: " <> original name

-- | Represents the bounds of /known/ $ApplicationRevisionSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ApplicationRevisionSortBy where
    minBound = FirstUsedTime
    maxBound = RegisterTime

instance Hashable     ApplicationRevisionSortBy
instance NFData       ApplicationRevisionSortBy
instance ToByteString ApplicationRevisionSortBy
instance ToQuery      ApplicationRevisionSortBy
instance ToHeader     ApplicationRevisionSortBy

instance ToJSON ApplicationRevisionSortBy where
    toJSON = toJSONText
