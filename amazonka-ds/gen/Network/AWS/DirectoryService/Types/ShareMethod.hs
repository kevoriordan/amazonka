{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ShareMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.ShareMethod (
  ShareMethod (
    ..
    , Handshake
    , Organizations
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ShareMethod = ShareMethod' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Handshake :: ShareMethod
pattern Handshake = ShareMethod' "HANDSHAKE"

pattern Organizations :: ShareMethod
pattern Organizations = ShareMethod' "ORGANIZATIONS"

{-# COMPLETE
  Handshake,
  Organizations,
  ShareMethod' #-}

instance FromText ShareMethod where
    parser = (ShareMethod' . mk) <$> takeText

instance ToText ShareMethod where
    toText (ShareMethod' ci) = original ci

-- | Represents an enum of /known/ $ShareMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ShareMethod where
    toEnum i = case i of
        0 -> Handshake
        1 -> Organizations
        _ -> (error . showText) $ "Unknown index for ShareMethod: " <> toText i
    fromEnum x = case x of
        Handshake -> 0
        Organizations -> 1
        ShareMethod' name -> (error . showText) $ "Unknown ShareMethod: " <> original name

-- | Represents the bounds of /known/ $ShareMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ShareMethod where
    minBound = Handshake
    maxBound = Organizations

instance Hashable     ShareMethod
instance NFData       ShareMethod
instance ToByteString ShareMethod
instance ToQuery      ShareMethod
instance ToHeader     ShareMethod

instance ToJSON ShareMethod where
    toJSON = toJSONText

instance FromJSON ShareMethod where
    parseJSON = parseJSONText "ShareMethod"
