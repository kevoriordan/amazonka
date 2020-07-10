{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.QueryErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.QueryErrorCode (
  QueryErrorCode (
    ..
    , CloudformationStackInactive
    , CloudformationStackNotExisting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data QueryErrorCode = QueryErrorCode' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern CloudformationStackInactive :: QueryErrorCode
pattern CloudformationStackInactive = QueryErrorCode' "CLOUDFORMATION_STACK_INACTIVE"

pattern CloudformationStackNotExisting :: QueryErrorCode
pattern CloudformationStackNotExisting = QueryErrorCode' "CLOUDFORMATION_STACK_NOT_EXISTING"

{-# COMPLETE
  CloudformationStackInactive,
  CloudformationStackNotExisting,
  QueryErrorCode' #-}

instance FromText QueryErrorCode where
    parser = (QueryErrorCode' . mk) <$> takeText

instance ToText QueryErrorCode where
    toText (QueryErrorCode' ci) = original ci

-- | Represents an enum of /known/ $QueryErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum QueryErrorCode where
    toEnum i = case i of
        0 -> CloudformationStackInactive
        1 -> CloudformationStackNotExisting
        _ -> (error . showText) $ "Unknown index for QueryErrorCode: " <> toText i
    fromEnum x = case x of
        CloudformationStackInactive -> 0
        CloudformationStackNotExisting -> 1
        QueryErrorCode' name -> (error . showText) $ "Unknown QueryErrorCode: " <> original name

-- | Represents the bounds of /known/ $QueryErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded QueryErrorCode where
    minBound = CloudformationStackInactive
    maxBound = CloudformationStackNotExisting

instance Hashable     QueryErrorCode
instance NFData       QueryErrorCode
instance ToByteString QueryErrorCode
instance ToQuery      QueryErrorCode
instance ToHeader     QueryErrorCode

instance FromJSON QueryErrorCode where
    parseJSON = parseJSONText "QueryErrorCode"
