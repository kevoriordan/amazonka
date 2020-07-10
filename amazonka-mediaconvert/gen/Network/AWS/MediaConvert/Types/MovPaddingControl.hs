{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovPaddingControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MovPaddingControl (
  MovPaddingControl (
    ..
    , MPCNone
    , MPCOmneon
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | If set to OMNEON, inserts Omneon-compatible padding
data MovPaddingControl = MovPaddingControl' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern MPCNone :: MovPaddingControl
pattern MPCNone = MovPaddingControl' "NONE"

pattern MPCOmneon :: MovPaddingControl
pattern MPCOmneon = MovPaddingControl' "OMNEON"

{-# COMPLETE
  MPCNone,
  MPCOmneon,
  MovPaddingControl' #-}

instance FromText MovPaddingControl where
    parser = (MovPaddingControl' . mk) <$> takeText

instance ToText MovPaddingControl where
    toText (MovPaddingControl' ci) = original ci

-- | Represents an enum of /known/ $MovPaddingControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MovPaddingControl where
    toEnum i = case i of
        0 -> MPCNone
        1 -> MPCOmneon
        _ -> (error . showText) $ "Unknown index for MovPaddingControl: " <> toText i
    fromEnum x = case x of
        MPCNone -> 0
        MPCOmneon -> 1
        MovPaddingControl' name -> (error . showText) $ "Unknown MovPaddingControl: " <> original name

-- | Represents the bounds of /known/ $MovPaddingControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MovPaddingControl where
    minBound = MPCNone
    maxBound = MPCOmneon

instance Hashable     MovPaddingControl
instance NFData       MovPaddingControl
instance ToByteString MovPaddingControl
instance ToQuery      MovPaddingControl
instance ToHeader     MovPaddingControl

instance ToJSON MovPaddingControl where
    toJSON = toJSONText

instance FromJSON MovPaddingControl where
    parseJSON = parseJSONText "MovPaddingControl"
