{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotificationTransport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.NotificationTransport (
  NotificationTransport (
    ..
    , Email
    , SNS
    , Sqs
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotificationTransport = NotificationTransport' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Email :: NotificationTransport
pattern Email = NotificationTransport' "Email"

pattern SNS :: NotificationTransport
pattern SNS = NotificationTransport' "SNS"

pattern Sqs :: NotificationTransport
pattern Sqs = NotificationTransport' "SQS"

{-# COMPLETE
  Email,
  SNS,
  Sqs,
  NotificationTransport' #-}

instance FromText NotificationTransport where
    parser = (NotificationTransport' . mk) <$> takeText

instance ToText NotificationTransport where
    toText (NotificationTransport' ci) = original ci

-- | Represents an enum of /known/ $NotificationTransport.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotificationTransport where
    toEnum i = case i of
        0 -> Email
        1 -> SNS
        2 -> Sqs
        _ -> (error . showText) $ "Unknown index for NotificationTransport: " <> toText i
    fromEnum x = case x of
        Email -> 0
        SNS -> 1
        Sqs -> 2
        NotificationTransport' name -> (error . showText) $ "Unknown NotificationTransport: " <> original name

-- | Represents the bounds of /known/ $NotificationTransport.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotificationTransport where
    minBound = Email
    maxBound = Sqs

instance Hashable     NotificationTransport
instance NFData       NotificationTransport
instance ToByteString NotificationTransport
instance ToQuery      NotificationTransport
instance ToHeader     NotificationTransport

instance ToJSON NotificationTransport where
    toJSON = toJSONText
