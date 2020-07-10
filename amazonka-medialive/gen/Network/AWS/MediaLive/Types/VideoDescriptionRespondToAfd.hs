{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd (
  VideoDescriptionRespondToAfd (
    ..
    , None
    , Passthrough
    , Respond
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Video Description Respond To Afd
data VideoDescriptionRespondToAfd = VideoDescriptionRespondToAfd' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern None :: VideoDescriptionRespondToAfd
pattern None = VideoDescriptionRespondToAfd' "NONE"

pattern Passthrough :: VideoDescriptionRespondToAfd
pattern Passthrough = VideoDescriptionRespondToAfd' "PASSTHROUGH"

pattern Respond :: VideoDescriptionRespondToAfd
pattern Respond = VideoDescriptionRespondToAfd' "RESPOND"

{-# COMPLETE
  None,
  Passthrough,
  Respond,
  VideoDescriptionRespondToAfd' #-}

instance FromText VideoDescriptionRespondToAfd where
    parser = (VideoDescriptionRespondToAfd' . mk) <$> takeText

instance ToText VideoDescriptionRespondToAfd where
    toText (VideoDescriptionRespondToAfd' ci) = original ci

-- | Represents an enum of /known/ $VideoDescriptionRespondToAfd.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VideoDescriptionRespondToAfd where
    toEnum i = case i of
        0 -> None
        1 -> Passthrough
        2 -> Respond
        _ -> (error . showText) $ "Unknown index for VideoDescriptionRespondToAfd: " <> toText i
    fromEnum x = case x of
        None -> 0
        Passthrough -> 1
        Respond -> 2
        VideoDescriptionRespondToAfd' name -> (error . showText) $ "Unknown VideoDescriptionRespondToAfd: " <> original name

-- | Represents the bounds of /known/ $VideoDescriptionRespondToAfd.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VideoDescriptionRespondToAfd where
    minBound = None
    maxBound = Respond

instance Hashable     VideoDescriptionRespondToAfd
instance NFData       VideoDescriptionRespondToAfd
instance ToByteString VideoDescriptionRespondToAfd
instance ToQuery      VideoDescriptionRespondToAfd
instance ToHeader     VideoDescriptionRespondToAfd

instance ToJSON VideoDescriptionRespondToAfd where
    toJSON = toJSONText

instance FromJSON VideoDescriptionRespondToAfd where
    parseJSON = parseJSONText "VideoDescriptionRespondToAfd"
