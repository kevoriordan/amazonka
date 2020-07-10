{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FixedAfd
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.FixedAfd (
  FixedAfd (
    ..
    , Afd0000
    , Afd0010
    , Afd0011
    , Afd0100
    , Afd1000
    , Afd1001
    , Afd1010
    , Afd1011
    , Afd1101
    , Afd1110
    , Afd1111
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Fixed Afd
data FixedAfd = FixedAfd' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Afd0000 :: FixedAfd
pattern Afd0000 = FixedAfd' "AFD_0000"

pattern Afd0010 :: FixedAfd
pattern Afd0010 = FixedAfd' "AFD_0010"

pattern Afd0011 :: FixedAfd
pattern Afd0011 = FixedAfd' "AFD_0011"

pattern Afd0100 :: FixedAfd
pattern Afd0100 = FixedAfd' "AFD_0100"

pattern Afd1000 :: FixedAfd
pattern Afd1000 = FixedAfd' "AFD_1000"

pattern Afd1001 :: FixedAfd
pattern Afd1001 = FixedAfd' "AFD_1001"

pattern Afd1010 :: FixedAfd
pattern Afd1010 = FixedAfd' "AFD_1010"

pattern Afd1011 :: FixedAfd
pattern Afd1011 = FixedAfd' "AFD_1011"

pattern Afd1101 :: FixedAfd
pattern Afd1101 = FixedAfd' "AFD_1101"

pattern Afd1110 :: FixedAfd
pattern Afd1110 = FixedAfd' "AFD_1110"

pattern Afd1111 :: FixedAfd
pattern Afd1111 = FixedAfd' "AFD_1111"

{-# COMPLETE
  Afd0000,
  Afd0010,
  Afd0011,
  Afd0100,
  Afd1000,
  Afd1001,
  Afd1010,
  Afd1011,
  Afd1101,
  Afd1110,
  Afd1111,
  FixedAfd' #-}

instance FromText FixedAfd where
    parser = (FixedAfd' . mk) <$> takeText

instance ToText FixedAfd where
    toText (FixedAfd' ci) = original ci

-- | Represents an enum of /known/ $FixedAfd.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FixedAfd where
    toEnum i = case i of
        0 -> Afd0000
        1 -> Afd0010
        2 -> Afd0011
        3 -> Afd0100
        4 -> Afd1000
        5 -> Afd1001
        6 -> Afd1010
        7 -> Afd1011
        8 -> Afd1101
        9 -> Afd1110
        10 -> Afd1111
        _ -> (error . showText) $ "Unknown index for FixedAfd: " <> toText i
    fromEnum x = case x of
        Afd0000 -> 0
        Afd0010 -> 1
        Afd0011 -> 2
        Afd0100 -> 3
        Afd1000 -> 4
        Afd1001 -> 5
        Afd1010 -> 6
        Afd1011 -> 7
        Afd1101 -> 8
        Afd1110 -> 9
        Afd1111 -> 10
        FixedAfd' name -> (error . showText) $ "Unknown FixedAfd: " <> original name

-- | Represents the bounds of /known/ $FixedAfd.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FixedAfd where
    minBound = Afd0000
    maxBound = Afd1111

instance Hashable     FixedAfd
instance NFData       FixedAfd
instance ToByteString FixedAfd
instance ToQuery      FixedAfd
instance ToHeader     FixedAfd

instance ToJSON FixedAfd where
    toJSON = toJSONText

instance FromJSON FixedAfd where
    parseJSON = parseJSONText "FixedAfd"
