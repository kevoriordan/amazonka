{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ProtectionPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.ProtectionPolicy (
  ProtectionPolicy (
    ..
    , PPFullProtection
    , PPNoProtection
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProtectionPolicy = ProtectionPolicy' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern PPFullProtection :: ProtectionPolicy
pattern PPFullProtection = ProtectionPolicy' "FullProtection"

pattern PPNoProtection :: ProtectionPolicy
pattern PPNoProtection = ProtectionPolicy' "NoProtection"

{-# COMPLETE
  PPFullProtection,
  PPNoProtection,
  ProtectionPolicy' #-}

instance FromText ProtectionPolicy where
    parser = (ProtectionPolicy' . mk) <$> takeText

instance ToText ProtectionPolicy where
    toText (ProtectionPolicy' ci) = original ci

-- | Represents an enum of /known/ $ProtectionPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProtectionPolicy where
    toEnum i = case i of
        0 -> PPFullProtection
        1 -> PPNoProtection
        _ -> (error . showText) $ "Unknown index for ProtectionPolicy: " <> toText i
    fromEnum x = case x of
        PPFullProtection -> 0
        PPNoProtection -> 1
        ProtectionPolicy' name -> (error . showText) $ "Unknown ProtectionPolicy: " <> original name

-- | Represents the bounds of /known/ $ProtectionPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProtectionPolicy where
    minBound = PPFullProtection
    maxBound = PPNoProtection

instance Hashable     ProtectionPolicy
instance NFData       ProtectionPolicy
instance ToByteString ProtectionPolicy
instance ToQuery      ProtectionPolicy
instance ToHeader     ProtectionPolicy

instance ToJSON ProtectionPolicy where
    toJSON = toJSONText

instance FromJSON ProtectionPolicy where
    parseJSON = parseJSONText "ProtectionPolicy"
