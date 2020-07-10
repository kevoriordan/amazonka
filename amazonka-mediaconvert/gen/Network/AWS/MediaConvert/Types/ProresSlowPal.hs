{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresSlowPal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ProresSlowPal (
  ProresSlowPal (
    ..
    , PSPDisabled
    , PSPEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
data ProresSlowPal = ProresSlowPal' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern PSPDisabled :: ProresSlowPal
pattern PSPDisabled = ProresSlowPal' "DISABLED"

pattern PSPEnabled :: ProresSlowPal
pattern PSPEnabled = ProresSlowPal' "ENABLED"

{-# COMPLETE
  PSPDisabled,
  PSPEnabled,
  ProresSlowPal' #-}

instance FromText ProresSlowPal where
    parser = (ProresSlowPal' . mk) <$> takeText

instance ToText ProresSlowPal where
    toText (ProresSlowPal' ci) = original ci

-- | Represents an enum of /known/ $ProresSlowPal.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProresSlowPal where
    toEnum i = case i of
        0 -> PSPDisabled
        1 -> PSPEnabled
        _ -> (error . showText) $ "Unknown index for ProresSlowPal: " <> toText i
    fromEnum x = case x of
        PSPDisabled -> 0
        PSPEnabled -> 1
        ProresSlowPal' name -> (error . showText) $ "Unknown ProresSlowPal: " <> original name

-- | Represents the bounds of /known/ $ProresSlowPal.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProresSlowPal where
    minBound = PSPDisabled
    maxBound = PSPEnabled

instance Hashable     ProresSlowPal
instance NFData       ProresSlowPal
instance ToByteString ProresSlowPal
instance ToQuery      ProresSlowPal
instance ToHeader     ProresSlowPal

instance ToJSON ProresSlowPal where
    toJSON = toJSONText

instance FromJSON ProresSlowPal where
    parseJSON = parseJSONText "ProresSlowPal"
