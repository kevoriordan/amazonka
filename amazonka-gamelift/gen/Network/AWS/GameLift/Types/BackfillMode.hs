{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.BackfillMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.BackfillMode (
  BackfillMode (
    ..
    , Automatic
    , Manual
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BackfillMode = BackfillMode' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Automatic :: BackfillMode
pattern Automatic = BackfillMode' "AUTOMATIC"

pattern Manual :: BackfillMode
pattern Manual = BackfillMode' "MANUAL"

{-# COMPLETE
  Automatic,
  Manual,
  BackfillMode' #-}

instance FromText BackfillMode where
    parser = (BackfillMode' . mk) <$> takeText

instance ToText BackfillMode where
    toText (BackfillMode' ci) = original ci

-- | Represents an enum of /known/ $BackfillMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BackfillMode where
    toEnum i = case i of
        0 -> Automatic
        1 -> Manual
        _ -> (error . showText) $ "Unknown index for BackfillMode: " <> toText i
    fromEnum x = case x of
        Automatic -> 0
        Manual -> 1
        BackfillMode' name -> (error . showText) $ "Unknown BackfillMode: " <> original name

-- | Represents the bounds of /known/ $BackfillMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BackfillMode where
    minBound = Automatic
    maxBound = Manual

instance Hashable     BackfillMode
instance NFData       BackfillMode
instance ToByteString BackfillMode
instance ToQuery      BackfillMode
instance ToHeader     BackfillMode

instance ToJSON BackfillMode where
    toJSON = toJSONText

instance FromJSON BackfillMode where
    parseJSON = parseJSONText "BackfillMode"
