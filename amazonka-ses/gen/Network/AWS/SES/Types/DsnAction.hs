{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.DsnAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.DsnAction (
  DsnAction (
    ..
    , DADelayed
    , DADelivered
    , DAExpanded
    , DAFailed
    , DARelayed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DsnAction = DsnAction' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern DADelayed :: DsnAction
pattern DADelayed = DsnAction' "delayed"

pattern DADelivered :: DsnAction
pattern DADelivered = DsnAction' "delivered"

pattern DAExpanded :: DsnAction
pattern DAExpanded = DsnAction' "expanded"

pattern DAFailed :: DsnAction
pattern DAFailed = DsnAction' "failed"

pattern DARelayed :: DsnAction
pattern DARelayed = DsnAction' "relayed"

{-# COMPLETE
  DADelayed,
  DADelivered,
  DAExpanded,
  DAFailed,
  DARelayed,
  DsnAction' #-}

instance FromText DsnAction where
    parser = (DsnAction' . mk) <$> takeText

instance ToText DsnAction where
    toText (DsnAction' ci) = original ci

-- | Represents an enum of /known/ $DsnAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DsnAction where
    toEnum i = case i of
        0 -> DADelayed
        1 -> DADelivered
        2 -> DAExpanded
        3 -> DAFailed
        4 -> DARelayed
        _ -> (error . showText) $ "Unknown index for DsnAction: " <> toText i
    fromEnum x = case x of
        DADelayed -> 0
        DADelivered -> 1
        DAExpanded -> 2
        DAFailed -> 3
        DARelayed -> 4
        DsnAction' name -> (error . showText) $ "Unknown DsnAction: " <> original name

-- | Represents the bounds of /known/ $DsnAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DsnAction where
    minBound = DADelayed
    maxBound = DARelayed

instance Hashable     DsnAction
instance NFData       DsnAction
instance ToByteString DsnAction
instance ToQuery      DsnAction
instance ToHeader     DsnAction
