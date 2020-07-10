{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.PlaylistType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.PlaylistType (
  PlaylistType (
    ..
    , PTEvent
    , PTNone
    , PTVod
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PlaylistType = PlaylistType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern PTEvent :: PlaylistType
pattern PTEvent = PlaylistType' "EVENT"

pattern PTNone :: PlaylistType
pattern PTNone = PlaylistType' "NONE"

pattern PTVod :: PlaylistType
pattern PTVod = PlaylistType' "VOD"

{-# COMPLETE
  PTEvent,
  PTNone,
  PTVod,
  PlaylistType' #-}

instance FromText PlaylistType where
    parser = (PlaylistType' . mk) <$> takeText

instance ToText PlaylistType where
    toText (PlaylistType' ci) = original ci

-- | Represents an enum of /known/ $PlaylistType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PlaylistType where
    toEnum i = case i of
        0 -> PTEvent
        1 -> PTNone
        2 -> PTVod
        _ -> (error . showText) $ "Unknown index for PlaylistType: " <> toText i
    fromEnum x = case x of
        PTEvent -> 0
        PTNone -> 1
        PTVod -> 2
        PlaylistType' name -> (error . showText) $ "Unknown PlaylistType: " <> original name

-- | Represents the bounds of /known/ $PlaylistType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PlaylistType where
    minBound = PTEvent
    maxBound = PTVod

instance Hashable     PlaylistType
instance NFData       PlaylistType
instance ToByteString PlaylistType
instance ToQuery      PlaylistType
instance ToHeader     PlaylistType

instance ToJSON PlaylistType where
    toJSON = toJSONText

instance FromJSON PlaylistType where
    parseJSON = parseJSONText "PlaylistType"
