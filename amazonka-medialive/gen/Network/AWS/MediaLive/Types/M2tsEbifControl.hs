{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsEbifControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsEbifControl (
  M2tsEbifControl (
    ..
    , MECNone
    , MECPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M2ts Ebif Control
data M2tsEbifControl = M2tsEbifControl' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern MECNone :: M2tsEbifControl
pattern MECNone = M2tsEbifControl' "NONE"

pattern MECPassthrough :: M2tsEbifControl
pattern MECPassthrough = M2tsEbifControl' "PASSTHROUGH"

{-# COMPLETE
  MECNone,
  MECPassthrough,
  M2tsEbifControl' #-}

instance FromText M2tsEbifControl where
    parser = (M2tsEbifControl' . mk) <$> takeText

instance ToText M2tsEbifControl where
    toText (M2tsEbifControl' ci) = original ci

-- | Represents an enum of /known/ $M2tsEbifControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsEbifControl where
    toEnum i = case i of
        0 -> MECNone
        1 -> MECPassthrough
        _ -> (error . showText) $ "Unknown index for M2tsEbifControl: " <> toText i
    fromEnum x = case x of
        MECNone -> 0
        MECPassthrough -> 1
        M2tsEbifControl' name -> (error . showText) $ "Unknown M2tsEbifControl: " <> original name

-- | Represents the bounds of /known/ $M2tsEbifControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsEbifControl where
    minBound = MECNone
    maxBound = MECPassthrough

instance Hashable     M2tsEbifControl
instance NFData       M2tsEbifControl
instance ToByteString M2tsEbifControl
instance ToQuery      M2tsEbifControl
instance ToHeader     M2tsEbifControl

instance ToJSON M2tsEbifControl where
    toJSON = toJSONText

instance FromJSON M2tsEbifControl where
    parseJSON = parseJSONText "M2tsEbifControl"
