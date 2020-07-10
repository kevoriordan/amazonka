{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.WebhookFilterType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.WebhookFilterType (
  WebhookFilterType (
    ..
    , ActorAccountId
    , BaseRef
    , Event
    , FilePath
    , HeadRef
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WebhookFilterType = WebhookFilterType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ActorAccountId :: WebhookFilterType
pattern ActorAccountId = WebhookFilterType' "ACTOR_ACCOUNT_ID"

pattern BaseRef :: WebhookFilterType
pattern BaseRef = WebhookFilterType' "BASE_REF"

pattern Event :: WebhookFilterType
pattern Event = WebhookFilterType' "EVENT"

pattern FilePath :: WebhookFilterType
pattern FilePath = WebhookFilterType' "FILE_PATH"

pattern HeadRef :: WebhookFilterType
pattern HeadRef = WebhookFilterType' "HEAD_REF"

{-# COMPLETE
  ActorAccountId,
  BaseRef,
  Event,
  FilePath,
  HeadRef,
  WebhookFilterType' #-}

instance FromText WebhookFilterType where
    parser = (WebhookFilterType' . mk) <$> takeText

instance ToText WebhookFilterType where
    toText (WebhookFilterType' ci) = original ci

-- | Represents an enum of /known/ $WebhookFilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WebhookFilterType where
    toEnum i = case i of
        0 -> ActorAccountId
        1 -> BaseRef
        2 -> Event
        3 -> FilePath
        4 -> HeadRef
        _ -> (error . showText) $ "Unknown index for WebhookFilterType: " <> toText i
    fromEnum x = case x of
        ActorAccountId -> 0
        BaseRef -> 1
        Event -> 2
        FilePath -> 3
        HeadRef -> 4
        WebhookFilterType' name -> (error . showText) $ "Unknown WebhookFilterType: " <> original name

-- | Represents the bounds of /known/ $WebhookFilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WebhookFilterType where
    minBound = ActorAccountId
    maxBound = HeadRef

instance Hashable     WebhookFilterType
instance NFData       WebhookFilterType
instance ToByteString WebhookFilterType
instance ToQuery      WebhookFilterType
instance ToHeader     WebhookFilterType

instance ToJSON WebhookFilterType where
    toJSON = toJSONText

instance FromJSON WebhookFilterType where
    parseJSON = parseJSONText "WebhookFilterType"
