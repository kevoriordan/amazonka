{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.SessionFilterKey (
  SessionFilterKey (
    ..
    , SFKInvokedAfter
    , SFKInvokedBefore
    , SFKOwner
    , SFKStatus
    , SFKTarget
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SessionFilterKey = SessionFilterKey' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern SFKInvokedAfter :: SessionFilterKey
pattern SFKInvokedAfter = SessionFilterKey' "InvokedAfter"

pattern SFKInvokedBefore :: SessionFilterKey
pattern SFKInvokedBefore = SessionFilterKey' "InvokedBefore"

pattern SFKOwner :: SessionFilterKey
pattern SFKOwner = SessionFilterKey' "Owner"

pattern SFKStatus :: SessionFilterKey
pattern SFKStatus = SessionFilterKey' "Status"

pattern SFKTarget :: SessionFilterKey
pattern SFKTarget = SessionFilterKey' "Target"

{-# COMPLETE
  SFKInvokedAfter,
  SFKInvokedBefore,
  SFKOwner,
  SFKStatus,
  SFKTarget,
  SessionFilterKey' #-}

instance FromText SessionFilterKey where
    parser = (SessionFilterKey' . mk) <$> takeText

instance ToText SessionFilterKey where
    toText (SessionFilterKey' ci) = original ci

-- | Represents an enum of /known/ $SessionFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SessionFilterKey where
    toEnum i = case i of
        0 -> SFKInvokedAfter
        1 -> SFKInvokedBefore
        2 -> SFKOwner
        3 -> SFKStatus
        4 -> SFKTarget
        _ -> (error . showText) $ "Unknown index for SessionFilterKey: " <> toText i
    fromEnum x = case x of
        SFKInvokedAfter -> 0
        SFKInvokedBefore -> 1
        SFKOwner -> 2
        SFKStatus -> 3
        SFKTarget -> 4
        SessionFilterKey' name -> (error . showText) $ "Unknown SessionFilterKey: " <> original name

-- | Represents the bounds of /known/ $SessionFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SessionFilterKey where
    minBound = SFKInvokedAfter
    maxBound = SFKTarget

instance Hashable     SessionFilterKey
instance NFData       SessionFilterKey
instance ToByteString SessionFilterKey
instance ToQuery      SessionFilterKey
instance ToHeader     SessionFilterKey

instance ToJSON SessionFilterKey where
    toJSON = toJSONText
