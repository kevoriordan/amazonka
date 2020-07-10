{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.InvocationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.InvocationType (
  InvocationType (
    ..
    , Event
    , RequestResponse
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InvocationType = InvocationType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Event :: InvocationType
pattern Event = InvocationType' "Event"

pattern RequestResponse :: InvocationType
pattern RequestResponse = InvocationType' "RequestResponse"

{-# COMPLETE
  Event,
  RequestResponse,
  InvocationType' #-}

instance FromText InvocationType where
    parser = (InvocationType' . mk) <$> takeText

instance ToText InvocationType where
    toText (InvocationType' ci) = original ci

-- | Represents an enum of /known/ $InvocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InvocationType where
    toEnum i = case i of
        0 -> Event
        1 -> RequestResponse
        _ -> (error . showText) $ "Unknown index for InvocationType: " <> toText i
    fromEnum x = case x of
        Event -> 0
        RequestResponse -> 1
        InvocationType' name -> (error . showText) $ "Unknown InvocationType: " <> original name

-- | Represents the bounds of /known/ $InvocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InvocationType where
    minBound = Event
    maxBound = RequestResponse

instance Hashable     InvocationType
instance NFData       InvocationType
instance ToByteString InvocationType
instance ToQuery      InvocationType
instance ToHeader     InvocationType

instance FromXML InvocationType where
    parseXML = parseXMLText "InvocationType"
