{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ReconnectEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.ReconnectEnum (
  ReconnectEnum (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReconnectEnum = ReconnectEnum' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Disabled :: ReconnectEnum
pattern Disabled = ReconnectEnum' "DISABLED"

pattern Enabled :: ReconnectEnum
pattern Enabled = ReconnectEnum' "ENABLED"

{-# COMPLETE
  Disabled,
  Enabled,
  ReconnectEnum' #-}

instance FromText ReconnectEnum where
    parser = (ReconnectEnum' . mk) <$> takeText

instance ToText ReconnectEnum where
    toText (ReconnectEnum' ci) = original ci

-- | Represents an enum of /known/ $ReconnectEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReconnectEnum where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for ReconnectEnum: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        ReconnectEnum' name -> (error . showText) $ "Unknown ReconnectEnum: " <> original name

-- | Represents the bounds of /known/ $ReconnectEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReconnectEnum where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     ReconnectEnum
instance NFData       ReconnectEnum
instance ToByteString ReconnectEnum
instance ToQuery      ReconnectEnum
instance ToHeader     ReconnectEnum

instance ToJSON ReconnectEnum where
    toJSON = toJSONText

instance FromJSON ReconnectEnum where
    parseJSON = parseJSONText "ReconnectEnum"
