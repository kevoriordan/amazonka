{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ChannelType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.ChannelType (
  ChannelType (
    ..
    , Facebook
    , Kik
    , Slack
    , TwilioSms
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChannelType = ChannelType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Facebook :: ChannelType
pattern Facebook = ChannelType' "Facebook"

pattern Kik :: ChannelType
pattern Kik = ChannelType' "Kik"

pattern Slack :: ChannelType
pattern Slack = ChannelType' "Slack"

pattern TwilioSms :: ChannelType
pattern TwilioSms = ChannelType' "Twilio-Sms"

{-# COMPLETE
  Facebook,
  Kik,
  Slack,
  TwilioSms,
  ChannelType' #-}

instance FromText ChannelType where
    parser = (ChannelType' . mk) <$> takeText

instance ToText ChannelType where
    toText (ChannelType' ci) = original ci

-- | Represents an enum of /known/ $ChannelType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChannelType where
    toEnum i = case i of
        0 -> Facebook
        1 -> Kik
        2 -> Slack
        3 -> TwilioSms
        _ -> (error . showText) $ "Unknown index for ChannelType: " <> toText i
    fromEnum x = case x of
        Facebook -> 0
        Kik -> 1
        Slack -> 2
        TwilioSms -> 3
        ChannelType' name -> (error . showText) $ "Unknown ChannelType: " <> original name

-- | Represents the bounds of /known/ $ChannelType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChannelType where
    minBound = Facebook
    maxBound = TwilioSms

instance Hashable     ChannelType
instance NFData       ChannelType
instance ToByteString ChannelType
instance ToQuery      ChannelType
instance ToHeader     ChannelType

instance FromJSON ChannelType where
    parseJSON = parseJSONText "ChannelType"
