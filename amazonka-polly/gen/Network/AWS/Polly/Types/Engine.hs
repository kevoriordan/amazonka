{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Engine
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.Engine (
  Engine (
    ..
    , Neural
    , Standard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Engine = Engine' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern Neural :: Engine
pattern Neural = Engine' "neural"

pattern Standard :: Engine
pattern Standard = Engine' "standard"

{-# COMPLETE
  Neural,
  Standard,
  Engine' #-}

instance FromText Engine where
    parser = (Engine' . mk) <$> takeText

instance ToText Engine where
    toText (Engine' ci) = original ci

-- | Represents an enum of /known/ $Engine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Engine where
    toEnum i = case i of
        0 -> Neural
        1 -> Standard
        _ -> (error . showText) $ "Unknown index for Engine: " <> toText i
    fromEnum x = case x of
        Neural -> 0
        Standard -> 1
        Engine' name -> (error . showText) $ "Unknown Engine: " <> original name

-- | Represents the bounds of /known/ $Engine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Engine where
    minBound = Neural
    maxBound = Standard

instance Hashable     Engine
instance NFData       Engine
instance ToByteString Engine
instance ToQuery      Engine
instance ToHeader     Engine

instance ToJSON Engine where
    toJSON = toJSONText

instance FromJSON Engine where
    parseJSON = parseJSONText "Engine"
