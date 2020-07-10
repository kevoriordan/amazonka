{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.OverrideStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.OverrideStatus (
  OverrideStatus (
    ..
    , Override
    , Revoke
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OverrideStatus = OverrideStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Override :: OverrideStatus
pattern Override = OverrideStatus' "OVERRIDE"

pattern Revoke :: OverrideStatus
pattern Revoke = OverrideStatus' "REVOKE"

{-# COMPLETE
  Override,
  Revoke,
  OverrideStatus' #-}

instance FromText OverrideStatus where
    parser = (OverrideStatus' . mk) <$> takeText

instance ToText OverrideStatus where
    toText (OverrideStatus' ci) = original ci

-- | Represents an enum of /known/ $OverrideStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OverrideStatus where
    toEnum i = case i of
        0 -> Override
        1 -> Revoke
        _ -> (error . showText) $ "Unknown index for OverrideStatus: " <> toText i
    fromEnum x = case x of
        Override -> 0
        Revoke -> 1
        OverrideStatus' name -> (error . showText) $ "Unknown OverrideStatus: " <> original name

-- | Represents the bounds of /known/ $OverrideStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OverrideStatus where
    minBound = Override
    maxBound = Revoke

instance Hashable     OverrideStatus
instance NFData       OverrideStatus
instance ToByteString OverrideStatus
instance ToQuery      OverrideStatus
instance ToHeader     OverrideStatus

instance ToJSON OverrideStatus where
    toJSON = toJSONText

instance FromJSON OverrideStatus where
    parseJSON = parseJSONText "OverrideStatus"
