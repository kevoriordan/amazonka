{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3CodingMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3CodingMode (
  Eac3CodingMode (
    ..
    , ECMCodingMode10
    , ECMCodingMode20
    , ECMCodingMode32
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Eac3 Coding Mode
data Eac3CodingMode = Eac3CodingMode' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ECMCodingMode10 :: Eac3CodingMode
pattern ECMCodingMode10 = Eac3CodingMode' "CODING_MODE_1_0"

pattern ECMCodingMode20 :: Eac3CodingMode
pattern ECMCodingMode20 = Eac3CodingMode' "CODING_MODE_2_0"

pattern ECMCodingMode32 :: Eac3CodingMode
pattern ECMCodingMode32 = Eac3CodingMode' "CODING_MODE_3_2"

{-# COMPLETE
  ECMCodingMode10,
  ECMCodingMode20,
  ECMCodingMode32,
  Eac3CodingMode' #-}

instance FromText Eac3CodingMode where
    parser = (Eac3CodingMode' . mk) <$> takeText

instance ToText Eac3CodingMode where
    toText (Eac3CodingMode' ci) = original ci

-- | Represents an enum of /known/ $Eac3CodingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3CodingMode where
    toEnum i = case i of
        0 -> ECMCodingMode10
        1 -> ECMCodingMode20
        2 -> ECMCodingMode32
        _ -> (error . showText) $ "Unknown index for Eac3CodingMode: " <> toText i
    fromEnum x = case x of
        ECMCodingMode10 -> 0
        ECMCodingMode20 -> 1
        ECMCodingMode32 -> 2
        Eac3CodingMode' name -> (error . showText) $ "Unknown Eac3CodingMode: " <> original name

-- | Represents the bounds of /known/ $Eac3CodingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3CodingMode where
    minBound = ECMCodingMode10
    maxBound = ECMCodingMode32

instance Hashable     Eac3CodingMode
instance NFData       Eac3CodingMode
instance ToByteString Eac3CodingMode
instance ToQuery      Eac3CodingMode
instance ToHeader     Eac3CodingMode

instance ToJSON Eac3CodingMode where
    toJSON = toJSONText

instance FromJSON Eac3CodingMode where
    parseJSON = parseJSONText "Eac3CodingMode"
