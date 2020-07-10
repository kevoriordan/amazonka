{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsEsRateInPes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M2tsEsRateInPes (
  M2tsEsRateInPes (
    ..
    , Exclude
    , Include
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Controls whether to include the ES Rate field in the PES header.
data M2tsEsRateInPes = M2tsEsRateInPes' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Exclude :: M2tsEsRateInPes
pattern Exclude = M2tsEsRateInPes' "EXCLUDE"

pattern Include :: M2tsEsRateInPes
pattern Include = M2tsEsRateInPes' "INCLUDE"

{-# COMPLETE
  Exclude,
  Include,
  M2tsEsRateInPes' #-}

instance FromText M2tsEsRateInPes where
    parser = (M2tsEsRateInPes' . mk) <$> takeText

instance ToText M2tsEsRateInPes where
    toText (M2tsEsRateInPes' ci) = original ci

-- | Represents an enum of /known/ $M2tsEsRateInPes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsEsRateInPes where
    toEnum i = case i of
        0 -> Exclude
        1 -> Include
        _ -> (error . showText) $ "Unknown index for M2tsEsRateInPes: " <> toText i
    fromEnum x = case x of
        Exclude -> 0
        Include -> 1
        M2tsEsRateInPes' name -> (error . showText) $ "Unknown M2tsEsRateInPes: " <> original name

-- | Represents the bounds of /known/ $M2tsEsRateInPes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsEsRateInPes where
    minBound = Exclude
    maxBound = Include

instance Hashable     M2tsEsRateInPes
instance NFData       M2tsEsRateInPes
instance ToByteString M2tsEsRateInPes
instance ToQuery      M2tsEsRateInPes
instance ToHeader     M2tsEsRateInPes

instance ToJSON M2tsEsRateInPes where
    toJSON = toJSONText

instance FromJSON M2tsEsRateInPes where
    parseJSON = parseJSONText "M2tsEsRateInPes"
