{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.EngineType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.EngineType (
  EngineType (
    ..
    , Activemq
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The type of broker engine. Note: Currently, Amazon MQ supports only ActiveMQ.
data EngineType = EngineType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Activemq :: EngineType
pattern Activemq = EngineType' "ACTIVEMQ"

{-# COMPLETE
  Activemq,
  EngineType' #-}

instance FromText EngineType where
    parser = (EngineType' . mk) <$> takeText

instance ToText EngineType where
    toText (EngineType' ci) = original ci

-- | Represents an enum of /known/ $EngineType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EngineType where
    toEnum i = case i of
        0 -> Activemq
        _ -> (error . showText) $ "Unknown index for EngineType: " <> toText i
    fromEnum x = case x of
        Activemq -> 0
        EngineType' name -> (error . showText) $ "Unknown EngineType: " <> original name

-- | Represents the bounds of /known/ $EngineType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EngineType where
    minBound = Activemq
    maxBound = Activemq

instance Hashable     EngineType
instance NFData       EngineType
instance ToByteString EngineType
instance ToQuery      EngineType
instance ToHeader     EngineType

instance ToJSON EngineType where
    toJSON = toJSONText

instance FromJSON EngineType where
    parseJSON = parseJSONText "EngineType"
