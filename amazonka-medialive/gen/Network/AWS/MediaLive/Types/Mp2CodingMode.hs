{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mp2CodingMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Mp2CodingMode (
  Mp2CodingMode (
    ..
    , MCMCodingMode10
    , MCMCodingMode20
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Mp2 Coding Mode
data Mp2CodingMode = Mp2CodingMode' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern MCMCodingMode10 :: Mp2CodingMode
pattern MCMCodingMode10 = Mp2CodingMode' "CODING_MODE_1_0"

pattern MCMCodingMode20 :: Mp2CodingMode
pattern MCMCodingMode20 = Mp2CodingMode' "CODING_MODE_2_0"

{-# COMPLETE
  MCMCodingMode10,
  MCMCodingMode20,
  Mp2CodingMode' #-}

instance FromText Mp2CodingMode where
    parser = (Mp2CodingMode' . mk) <$> takeText

instance ToText Mp2CodingMode where
    toText (Mp2CodingMode' ci) = original ci

-- | Represents an enum of /known/ $Mp2CodingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mp2CodingMode where
    toEnum i = case i of
        0 -> MCMCodingMode10
        1 -> MCMCodingMode20
        _ -> (error . showText) $ "Unknown index for Mp2CodingMode: " <> toText i
    fromEnum x = case x of
        MCMCodingMode10 -> 0
        MCMCodingMode20 -> 1
        Mp2CodingMode' name -> (error . showText) $ "Unknown Mp2CodingMode: " <> original name

-- | Represents the bounds of /known/ $Mp2CodingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mp2CodingMode where
    minBound = MCMCodingMode10
    maxBound = MCMCodingMode20

instance Hashable     Mp2CodingMode
instance NFData       Mp2CodingMode
instance ToByteString Mp2CodingMode
instance ToQuery      Mp2CodingMode
instance ToHeader     Mp2CodingMode

instance ToJSON Mp2CodingMode where
    toJSON = toJSONText

instance FromJSON Mp2CodingMode where
    parseJSON = parseJSONText "Mp2CodingMode"
