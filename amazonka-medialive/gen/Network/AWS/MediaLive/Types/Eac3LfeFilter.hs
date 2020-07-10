{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3LfeFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3LfeFilter (
  Eac3LfeFilter (
    ..
    , ELFDisabled
    , ELFEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Eac3 Lfe Filter
data Eac3LfeFilter = Eac3LfeFilter' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern ELFDisabled :: Eac3LfeFilter
pattern ELFDisabled = Eac3LfeFilter' "DISABLED"

pattern ELFEnabled :: Eac3LfeFilter
pattern ELFEnabled = Eac3LfeFilter' "ENABLED"

{-# COMPLETE
  ELFDisabled,
  ELFEnabled,
  Eac3LfeFilter' #-}

instance FromText Eac3LfeFilter where
    parser = (Eac3LfeFilter' . mk) <$> takeText

instance ToText Eac3LfeFilter where
    toText (Eac3LfeFilter' ci) = original ci

-- | Represents an enum of /known/ $Eac3LfeFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3LfeFilter where
    toEnum i = case i of
        0 -> ELFDisabled
        1 -> ELFEnabled
        _ -> (error . showText) $ "Unknown index for Eac3LfeFilter: " <> toText i
    fromEnum x = case x of
        ELFDisabled -> 0
        ELFEnabled -> 1
        Eac3LfeFilter' name -> (error . showText) $ "Unknown Eac3LfeFilter: " <> original name

-- | Represents the bounds of /known/ $Eac3LfeFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3LfeFilter where
    minBound = ELFDisabled
    maxBound = ELFEnabled

instance Hashable     Eac3LfeFilter
instance NFData       Eac3LfeFilter
instance ToByteString Eac3LfeFilter
instance ToQuery      Eac3LfeFilter
instance ToHeader     Eac3LfeFilter

instance ToJSON Eac3LfeFilter where
    toJSON = toJSONText

instance FromJSON Eac3LfeFilter where
    parseJSON = parseJSONText "Eac3LfeFilter"
