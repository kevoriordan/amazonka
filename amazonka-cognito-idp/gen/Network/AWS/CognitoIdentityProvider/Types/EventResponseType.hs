{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventResponseType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.EventResponseType (
  EventResponseType (
    ..
    , Failure
    , Success
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventResponseType = EventResponseType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Failure :: EventResponseType
pattern Failure = EventResponseType' "Failure"

pattern Success :: EventResponseType
pattern Success = EventResponseType' "Success"

{-# COMPLETE
  Failure,
  Success,
  EventResponseType' #-}

instance FromText EventResponseType where
    parser = (EventResponseType' . mk) <$> takeText

instance ToText EventResponseType where
    toText (EventResponseType' ci) = original ci

-- | Represents an enum of /known/ $EventResponseType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventResponseType where
    toEnum i = case i of
        0 -> Failure
        1 -> Success
        _ -> (error . showText) $ "Unknown index for EventResponseType: " <> toText i
    fromEnum x = case x of
        Failure -> 0
        Success -> 1
        EventResponseType' name -> (error . showText) $ "Unknown EventResponseType: " <> original name

-- | Represents the bounds of /known/ $EventResponseType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventResponseType where
    minBound = Failure
    maxBound = Success

instance Hashable     EventResponseType
instance NFData       EventResponseType
instance ToByteString EventResponseType
instance ToQuery      EventResponseType
instance ToHeader     EventResponseType

instance FromJSON EventResponseType where
    parseJSON = parseJSONText "EventResponseType"
