{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.StatusCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.StatusCode (
  StatusCode (
    ..
    , Failed
    , InProgress
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StatusCode = StatusCode' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Failed :: StatusCode
pattern Failed = StatusCode' "Failed"

pattern InProgress :: StatusCode
pattern InProgress = StatusCode' "InProgress"

pattern Succeeded :: StatusCode
pattern Succeeded = StatusCode' "Succeeded"

{-# COMPLETE
  Failed,
  InProgress,
  Succeeded,
  StatusCode' #-}

instance FromText StatusCode where
    parser = (StatusCode' . mk) <$> takeText

instance ToText StatusCode where
    toText (StatusCode' ci) = original ci

-- | Represents an enum of /known/ $StatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StatusCode where
    toEnum i = case i of
        0 -> Failed
        1 -> InProgress
        2 -> Succeeded
        _ -> (error . showText) $ "Unknown index for StatusCode: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        InProgress -> 1
        Succeeded -> 2
        StatusCode' name -> (error . showText) $ "Unknown StatusCode: " <> original name

-- | Represents the bounds of /known/ $StatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StatusCode where
    minBound = Failed
    maxBound = Succeeded

instance Hashable     StatusCode
instance NFData       StatusCode
instance ToByteString StatusCode
instance ToQuery      StatusCode
instance ToHeader     StatusCode

instance FromJSON StatusCode where
    parseJSON = parseJSONText "StatusCode"
