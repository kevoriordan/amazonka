{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsKlv
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsKlv (
  M2tsKlv (
    ..
    , MKNone
    , MKPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M2ts Klv
data M2tsKlv = M2tsKlv' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern MKNone :: M2tsKlv
pattern MKNone = M2tsKlv' "NONE"

pattern MKPassthrough :: M2tsKlv
pattern MKPassthrough = M2tsKlv' "PASSTHROUGH"

{-# COMPLETE
  MKNone,
  MKPassthrough,
  M2tsKlv' #-}

instance FromText M2tsKlv where
    parser = (M2tsKlv' . mk) <$> takeText

instance ToText M2tsKlv where
    toText (M2tsKlv' ci) = original ci

-- | Represents an enum of /known/ $M2tsKlv.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsKlv where
    toEnum i = case i of
        0 -> MKNone
        1 -> MKPassthrough
        _ -> (error . showText) $ "Unknown index for M2tsKlv: " <> toText i
    fromEnum x = case x of
        MKNone -> 0
        MKPassthrough -> 1
        M2tsKlv' name -> (error . showText) $ "Unknown M2tsKlv: " <> original name

-- | Represents the bounds of /known/ $M2tsKlv.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsKlv where
    minBound = MKNone
    maxBound = MKPassthrough

instance Hashable     M2tsKlv
instance NFData       M2tsKlv
instance ToByteString M2tsKlv
instance ToQuery      M2tsKlv
instance ToHeader     M2tsKlv

instance ToJSON M2tsKlv where
    toJSON = toJSONText

instance FromJSON M2tsKlv where
    parseJSON = parseJSONText "M2tsKlv"
