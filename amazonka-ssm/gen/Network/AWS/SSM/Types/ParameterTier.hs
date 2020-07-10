{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterTier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ParameterTier (
  ParameterTier (
    ..
    , Advanced
    , IntelligentTiering
    , Standard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ParameterTier = ParameterTier' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Advanced :: ParameterTier
pattern Advanced = ParameterTier' "Advanced"

pattern IntelligentTiering :: ParameterTier
pattern IntelligentTiering = ParameterTier' "Intelligent-Tiering"

pattern Standard :: ParameterTier
pattern Standard = ParameterTier' "Standard"

{-# COMPLETE
  Advanced,
  IntelligentTiering,
  Standard,
  ParameterTier' #-}

instance FromText ParameterTier where
    parser = (ParameterTier' . mk) <$> takeText

instance ToText ParameterTier where
    toText (ParameterTier' ci) = original ci

-- | Represents an enum of /known/ $ParameterTier.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ParameterTier where
    toEnum i = case i of
        0 -> Advanced
        1 -> IntelligentTiering
        2 -> Standard
        _ -> (error . showText) $ "Unknown index for ParameterTier: " <> toText i
    fromEnum x = case x of
        Advanced -> 0
        IntelligentTiering -> 1
        Standard -> 2
        ParameterTier' name -> (error . showText) $ "Unknown ParameterTier: " <> original name

-- | Represents the bounds of /known/ $ParameterTier.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ParameterTier where
    minBound = Advanced
    maxBound = Standard

instance Hashable     ParameterTier
instance NFData       ParameterTier
instance ToByteString ParameterTier
instance ToQuery      ParameterTier
instance ToHeader     ParameterTier

instance ToJSON ParameterTier where
    toJSON = toJSONText

instance FromJSON ParameterTier where
    parseJSON = parseJSONText "ParameterTier"
