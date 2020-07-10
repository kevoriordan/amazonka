{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AntiAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AntiAlias (
  AntiAlias (
    ..
    , AADisabled
    , AAEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The anti-alias filter is automatically applied to all outputs. The service no longer accepts the value DISABLED for AntiAlias. If you specify that in your job, the service will ignore the setting.
data AntiAlias = AntiAlias' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern AADisabled :: AntiAlias
pattern AADisabled = AntiAlias' "DISABLED"

pattern AAEnabled :: AntiAlias
pattern AAEnabled = AntiAlias' "ENABLED"

{-# COMPLETE
  AADisabled,
  AAEnabled,
  AntiAlias' #-}

instance FromText AntiAlias where
    parser = (AntiAlias' . mk) <$> takeText

instance ToText AntiAlias where
    toText (AntiAlias' ci) = original ci

-- | Represents an enum of /known/ $AntiAlias.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AntiAlias where
    toEnum i = case i of
        0 -> AADisabled
        1 -> AAEnabled
        _ -> (error . showText) $ "Unknown index for AntiAlias: " <> toText i
    fromEnum x = case x of
        AADisabled -> 0
        AAEnabled -> 1
        AntiAlias' name -> (error . showText) $ "Unknown AntiAlias: " <> original name

-- | Represents the bounds of /known/ $AntiAlias.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AntiAlias where
    minBound = AADisabled
    maxBound = AAEnabled

instance Hashable     AntiAlias
instance NFData       AntiAlias
instance ToByteString AntiAlias
instance ToQuery      AntiAlias
instance ToHeader     AntiAlias

instance ToJSON AntiAlias where
    toJSON = toJSONText

instance FromJSON AntiAlias where
    parseJSON = parseJSONText "AntiAlias"
