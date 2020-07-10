{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITAccessActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.HITAccessActions (
  HITAccessActions (
    ..
    , Accept
    , DiscoverPreviewAndAccept
    , PreviewAndAccept
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HITAccessActions = HITAccessActions' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Accept :: HITAccessActions
pattern Accept = HITAccessActions' "Accept"

pattern DiscoverPreviewAndAccept :: HITAccessActions
pattern DiscoverPreviewAndAccept = HITAccessActions' "DiscoverPreviewAndAccept"

pattern PreviewAndAccept :: HITAccessActions
pattern PreviewAndAccept = HITAccessActions' "PreviewAndAccept"

{-# COMPLETE
  Accept,
  DiscoverPreviewAndAccept,
  PreviewAndAccept,
  HITAccessActions' #-}

instance FromText HITAccessActions where
    parser = (HITAccessActions' . mk) <$> takeText

instance ToText HITAccessActions where
    toText (HITAccessActions' ci) = original ci

-- | Represents an enum of /known/ $HITAccessActions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HITAccessActions where
    toEnum i = case i of
        0 -> Accept
        1 -> DiscoverPreviewAndAccept
        2 -> PreviewAndAccept
        _ -> (error . showText) $ "Unknown index for HITAccessActions: " <> toText i
    fromEnum x = case x of
        Accept -> 0
        DiscoverPreviewAndAccept -> 1
        PreviewAndAccept -> 2
        HITAccessActions' name -> (error . showText) $ "Unknown HITAccessActions: " <> original name

-- | Represents the bounds of /known/ $HITAccessActions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HITAccessActions where
    minBound = Accept
    maxBound = PreviewAndAccept

instance Hashable     HITAccessActions
instance NFData       HITAccessActions
instance ToByteString HITAccessActions
instance ToQuery      HITAccessActions
instance ToHeader     HITAccessActions

instance ToJSON HITAccessActions where
    toJSON = toJSONText

instance FromJSON HITAccessActions where
    parseJSON = parseJSONText "HITAccessActions"
