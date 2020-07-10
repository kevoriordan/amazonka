{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.JoinSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.JoinSource (
  JoinSource (
    ..
    , JSInput
    , JSNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JoinSource = JoinSource' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern JSInput :: JoinSource
pattern JSInput = JoinSource' "Input"

pattern JSNone :: JoinSource
pattern JSNone = JoinSource' "None"

{-# COMPLETE
  JSInput,
  JSNone,
  JoinSource' #-}

instance FromText JoinSource where
    parser = (JoinSource' . mk) <$> takeText

instance ToText JoinSource where
    toText (JoinSource' ci) = original ci

-- | Represents an enum of /known/ $JoinSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JoinSource where
    toEnum i = case i of
        0 -> JSInput
        1 -> JSNone
        _ -> (error . showText) $ "Unknown index for JoinSource: " <> toText i
    fromEnum x = case x of
        JSInput -> 0
        JSNone -> 1
        JoinSource' name -> (error . showText) $ "Unknown JoinSource: " <> original name

-- | Represents the bounds of /known/ $JoinSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JoinSource where
    minBound = JSInput
    maxBound = JSNone

instance Hashable     JoinSource
instance NFData       JoinSource
instance ToByteString JoinSource
instance ToQuery      JoinSource
instance ToHeader     JoinSource

instance ToJSON JoinSource where
    toJSON = toJSONText

instance FromJSON JoinSource where
    parseJSON = parseJSONText "JoinSource"
