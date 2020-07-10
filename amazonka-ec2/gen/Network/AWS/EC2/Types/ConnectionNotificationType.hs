{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionNotificationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ConnectionNotificationType (
  ConnectionNotificationType (
    ..
    , Topic
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ConnectionNotificationType = ConnectionNotificationType' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern Topic :: ConnectionNotificationType
pattern Topic = ConnectionNotificationType' "Topic"

{-# COMPLETE
  Topic,
  ConnectionNotificationType' #-}

instance FromText ConnectionNotificationType where
    parser = (ConnectionNotificationType' . mk) <$> takeText

instance ToText ConnectionNotificationType where
    toText (ConnectionNotificationType' ci) = original ci

-- | Represents an enum of /known/ $ConnectionNotificationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConnectionNotificationType where
    toEnum i = case i of
        0 -> Topic
        _ -> (error . showText) $ "Unknown index for ConnectionNotificationType: " <> toText i
    fromEnum x = case x of
        Topic -> 0
        ConnectionNotificationType' name -> (error . showText) $ "Unknown ConnectionNotificationType: " <> original name

-- | Represents the bounds of /known/ $ConnectionNotificationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConnectionNotificationType where
    minBound = Topic
    maxBound = Topic

instance Hashable     ConnectionNotificationType
instance NFData       ConnectionNotificationType
instance ToByteString ConnectionNotificationType
instance ToQuery      ConnectionNotificationType
instance ToHeader     ConnectionNotificationType

instance FromXML ConnectionNotificationType where
    parseXML = parseXMLText "ConnectionNotificationType"
