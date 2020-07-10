{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Protocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Protocol (
  Protocol (
    ..
    , HTTP
    , HTTPS
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data Protocol = Protocol' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern HTTP :: Protocol
pattern HTTP = Protocol' "http"

pattern HTTPS :: Protocol
pattern HTTPS = Protocol' "https"

{-# COMPLETE
  HTTP,
  HTTPS,
  Protocol' #-}

instance FromText Protocol where
    parser = (Protocol' . mk) <$> takeText

instance ToText Protocol where
    toText (Protocol' ci) = original ci

-- | Represents an enum of /known/ $Protocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Protocol where
    toEnum i = case i of
        0 -> HTTP
        1 -> HTTPS
        _ -> (error . showText) $ "Unknown index for Protocol: " <> toText i
    fromEnum x = case x of
        HTTP -> 0
        HTTPS -> 1
        Protocol' name -> (error . showText) $ "Unknown Protocol: " <> original name

-- | Represents the bounds of /known/ $Protocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Protocol where
    minBound = HTTP
    maxBound = HTTPS

instance Hashable     Protocol
instance NFData       Protocol
instance ToByteString Protocol
instance ToQuery      Protocol
instance ToHeader     Protocol

instance FromXML Protocol where
    parseXML = parseXMLText "Protocol"

instance ToXML Protocol where
    toXML = toXMLText
