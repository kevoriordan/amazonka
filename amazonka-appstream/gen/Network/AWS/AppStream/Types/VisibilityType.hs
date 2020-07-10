{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.VisibilityType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.VisibilityType (
  VisibilityType (
    ..
    , Private
    , Public
    , Shared
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data VisibilityType = VisibilityType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Private :: VisibilityType
pattern Private = VisibilityType' "PRIVATE"

pattern Public :: VisibilityType
pattern Public = VisibilityType' "PUBLIC"

pattern Shared :: VisibilityType
pattern Shared = VisibilityType' "SHARED"

{-# COMPLETE
  Private,
  Public,
  Shared,
  VisibilityType' #-}

instance FromText VisibilityType where
    parser = (VisibilityType' . mk) <$> takeText

instance ToText VisibilityType where
    toText (VisibilityType' ci) = original ci

-- | Represents an enum of /known/ $VisibilityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VisibilityType where
    toEnum i = case i of
        0 -> Private
        1 -> Public
        2 -> Shared
        _ -> (error . showText) $ "Unknown index for VisibilityType: " <> toText i
    fromEnum x = case x of
        Private -> 0
        Public -> 1
        Shared -> 2
        VisibilityType' name -> (error . showText) $ "Unknown VisibilityType: " <> original name

-- | Represents the bounds of /known/ $VisibilityType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VisibilityType where
    minBound = Private
    maxBound = Shared

instance Hashable     VisibilityType
instance NFData       VisibilityType
instance ToByteString VisibilityType
instance ToQuery      VisibilityType
instance ToHeader     VisibilityType

instance ToJSON VisibilityType where
    toJSON = toJSONText

instance FromJSON VisibilityType where
    parseJSON = parseJSONText "VisibilityType"
