{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode (
  ScalingActivityStatusCode (
    ..
    , Failed
    , InProgress
    , Overridden
    , Pending
    , Successful
    , Unfulfilled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScalingActivityStatusCode = ScalingActivityStatusCode' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern Failed :: ScalingActivityStatusCode
pattern Failed = ScalingActivityStatusCode' "Failed"

pattern InProgress :: ScalingActivityStatusCode
pattern InProgress = ScalingActivityStatusCode' "InProgress"

pattern Overridden :: ScalingActivityStatusCode
pattern Overridden = ScalingActivityStatusCode' "Overridden"

pattern Pending :: ScalingActivityStatusCode
pattern Pending = ScalingActivityStatusCode' "Pending"

pattern Successful :: ScalingActivityStatusCode
pattern Successful = ScalingActivityStatusCode' "Successful"

pattern Unfulfilled :: ScalingActivityStatusCode
pattern Unfulfilled = ScalingActivityStatusCode' "Unfulfilled"

{-# COMPLETE
  Failed,
  InProgress,
  Overridden,
  Pending,
  Successful,
  Unfulfilled,
  ScalingActivityStatusCode' #-}

instance FromText ScalingActivityStatusCode where
    parser = (ScalingActivityStatusCode' . mk) <$> takeText

instance ToText ScalingActivityStatusCode where
    toText (ScalingActivityStatusCode' ci) = original ci

-- | Represents an enum of /known/ $ScalingActivityStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScalingActivityStatusCode where
    toEnum i = case i of
        0 -> Failed
        1 -> InProgress
        2 -> Overridden
        3 -> Pending
        4 -> Successful
        5 -> Unfulfilled
        _ -> (error . showText) $ "Unknown index for ScalingActivityStatusCode: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        InProgress -> 1
        Overridden -> 2
        Pending -> 3
        Successful -> 4
        Unfulfilled -> 5
        ScalingActivityStatusCode' name -> (error . showText) $ "Unknown ScalingActivityStatusCode: " <> original name

-- | Represents the bounds of /known/ $ScalingActivityStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScalingActivityStatusCode where
    minBound = Failed
    maxBound = Unfulfilled

instance Hashable     ScalingActivityStatusCode
instance NFData       ScalingActivityStatusCode
instance ToByteString ScalingActivityStatusCode
instance ToQuery      ScalingActivityStatusCode
instance ToHeader     ScalingActivityStatusCode

instance FromJSON ScalingActivityStatusCode where
    parseJSON = parseJSONText "ScalingActivityStatusCode"
