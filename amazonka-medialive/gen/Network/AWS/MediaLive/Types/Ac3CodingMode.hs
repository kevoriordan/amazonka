{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3CodingMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Ac3CodingMode (
  Ac3CodingMode (
    ..
    , CodingMode10
    , CodingMode11
    , CodingMode20
    , CodingMode32Lfe
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Ac3 Coding Mode
data Ac3CodingMode = Ac3CodingMode' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern CodingMode10 :: Ac3CodingMode
pattern CodingMode10 = Ac3CodingMode' "CODING_MODE_1_0"

pattern CodingMode11 :: Ac3CodingMode
pattern CodingMode11 = Ac3CodingMode' "CODING_MODE_1_1"

pattern CodingMode20 :: Ac3CodingMode
pattern CodingMode20 = Ac3CodingMode' "CODING_MODE_2_0"

pattern CodingMode32Lfe :: Ac3CodingMode
pattern CodingMode32Lfe = Ac3CodingMode' "CODING_MODE_3_2_LFE"

{-# COMPLETE
  CodingMode10,
  CodingMode11,
  CodingMode20,
  CodingMode32Lfe,
  Ac3CodingMode' #-}

instance FromText Ac3CodingMode where
    parser = (Ac3CodingMode' . mk) <$> takeText

instance ToText Ac3CodingMode where
    toText (Ac3CodingMode' ci) = original ci

-- | Represents an enum of /known/ $Ac3CodingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Ac3CodingMode where
    toEnum i = case i of
        0 -> CodingMode10
        1 -> CodingMode11
        2 -> CodingMode20
        3 -> CodingMode32Lfe
        _ -> (error . showText) $ "Unknown index for Ac3CodingMode: " <> toText i
    fromEnum x = case x of
        CodingMode10 -> 0
        CodingMode11 -> 1
        CodingMode20 -> 2
        CodingMode32Lfe -> 3
        Ac3CodingMode' name -> (error . showText) $ "Unknown Ac3CodingMode: " <> original name

-- | Represents the bounds of /known/ $Ac3CodingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Ac3CodingMode where
    minBound = CodingMode10
    maxBound = CodingMode32Lfe

instance Hashable     Ac3CodingMode
instance NFData       Ac3CodingMode
instance ToByteString Ac3CodingMode
instance ToQuery      Ac3CodingMode
instance ToHeader     Ac3CodingMode

instance ToJSON Ac3CodingMode where
    toJSON = toJSONText

instance FromJSON Ac3CodingMode where
    parseJSON = parseJSONText "Ac3CodingMode"
