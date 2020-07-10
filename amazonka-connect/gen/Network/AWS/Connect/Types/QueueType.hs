{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.QueueType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.QueueType (
  QueueType (
    ..
    , Agent
    , Standard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data QueueType = QueueType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Agent :: QueueType
pattern Agent = QueueType' "AGENT"

pattern Standard :: QueueType
pattern Standard = QueueType' "STANDARD"

{-# COMPLETE
  Agent,
  Standard,
  QueueType' #-}

instance FromText QueueType where
    parser = (QueueType' . mk) <$> takeText

instance ToText QueueType where
    toText (QueueType' ci) = original ci

-- | Represents an enum of /known/ $QueueType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum QueueType where
    toEnum i = case i of
        0 -> Agent
        1 -> Standard
        _ -> (error . showText) $ "Unknown index for QueueType: " <> toText i
    fromEnum x = case x of
        Agent -> 0
        Standard -> 1
        QueueType' name -> (error . showText) $ "Unknown QueueType: " <> original name

-- | Represents the bounds of /known/ $QueueType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded QueueType where
    minBound = Agent
    maxBound = Standard

instance Hashable     QueueType
instance NFData       QueueType
instance ToByteString QueueType
instance ToQuery      QueueType
instance ToHeader     QueueType

instance ToJSON QueueType where
    toJSON = toJSONText

instance FromJSON QueueType where
    parseJSON = parseJSONText "QueueType"
