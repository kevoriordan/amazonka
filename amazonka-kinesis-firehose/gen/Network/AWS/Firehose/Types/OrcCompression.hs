{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OrcCompression
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.OrcCompression (
  OrcCompression (
    ..
    , OCNone
    , OCSnappy
    , OCZlib
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrcCompression = OrcCompression' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern OCNone :: OrcCompression
pattern OCNone = OrcCompression' "NONE"

pattern OCSnappy :: OrcCompression
pattern OCSnappy = OrcCompression' "SNAPPY"

pattern OCZlib :: OrcCompression
pattern OCZlib = OrcCompression' "ZLIB"

{-# COMPLETE
  OCNone,
  OCSnappy,
  OCZlib,
  OrcCompression' #-}

instance FromText OrcCompression where
    parser = (OrcCompression' . mk) <$> takeText

instance ToText OrcCompression where
    toText (OrcCompression' ci) = original ci

-- | Represents an enum of /known/ $OrcCompression.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrcCompression where
    toEnum i = case i of
        0 -> OCNone
        1 -> OCSnappy
        2 -> OCZlib
        _ -> (error . showText) $ "Unknown index for OrcCompression: " <> toText i
    fromEnum x = case x of
        OCNone -> 0
        OCSnappy -> 1
        OCZlib -> 2
        OrcCompression' name -> (error . showText) $ "Unknown OrcCompression: " <> original name

-- | Represents the bounds of /known/ $OrcCompression.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrcCompression where
    minBound = OCNone
    maxBound = OCZlib

instance Hashable     OrcCompression
instance NFData       OrcCompression
instance ToByteString OrcCompression
instance ToQuery      OrcCompression
instance ToHeader     OrcCompression

instance ToJSON OrcCompression where
    toJSON = toJSONText

instance FromJSON OrcCompression where
    parseJSON = parseJSONText "OrcCompression"
