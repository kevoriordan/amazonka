{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JQState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.JQState (
  JQState (
    ..
    , JQSDisabled
    , JQSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JQState = JQState' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern JQSDisabled :: JQState
pattern JQSDisabled = JQState' "DISABLED"

pattern JQSEnabled :: JQState
pattern JQSEnabled = JQState' "ENABLED"

{-# COMPLETE
  JQSDisabled,
  JQSEnabled,
  JQState' #-}

instance FromText JQState where
    parser = (JQState' . mk) <$> takeText

instance ToText JQState where
    toText (JQState' ci) = original ci

-- | Represents an enum of /known/ $JQState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JQState where
    toEnum i = case i of
        0 -> JQSDisabled
        1 -> JQSEnabled
        _ -> (error . showText) $ "Unknown index for JQState: " <> toText i
    fromEnum x = case x of
        JQSDisabled -> 0
        JQSEnabled -> 1
        JQState' name -> (error . showText) $ "Unknown JQState: " <> original name

-- | Represents the bounds of /known/ $JQState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JQState where
    minBound = JQSDisabled
    maxBound = JQSEnabled

instance Hashable     JQState
instance NFData       JQState
instance ToByteString JQState
instance ToQuery      JQState
instance ToHeader     JQState

instance ToJSON JQState where
    toJSON = toJSONText

instance FromJSON JQState where
    parseJSON = parseJSONText "JQState"
