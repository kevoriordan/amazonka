{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.FieldLogLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.FieldLogLevel (
  FieldLogLevel (
    ..
    , FLLAll
    , FLLError'
    , FLLNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FieldLogLevel = FieldLogLevel' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern FLLAll :: FieldLogLevel
pattern FLLAll = FieldLogLevel' "ALL"

pattern FLLError' :: FieldLogLevel
pattern FLLError' = FieldLogLevel' "ERROR"

pattern FLLNone :: FieldLogLevel
pattern FLLNone = FieldLogLevel' "NONE"

{-# COMPLETE
  FLLAll,
  FLLError',
  FLLNone,
  FieldLogLevel' #-}

instance FromText FieldLogLevel where
    parser = (FieldLogLevel' . mk) <$> takeText

instance ToText FieldLogLevel where
    toText (FieldLogLevel' ci) = original ci

-- | Represents an enum of /known/ $FieldLogLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FieldLogLevel where
    toEnum i = case i of
        0 -> FLLAll
        1 -> FLLError'
        2 -> FLLNone
        _ -> (error . showText) $ "Unknown index for FieldLogLevel: " <> toText i
    fromEnum x = case x of
        FLLAll -> 0
        FLLError' -> 1
        FLLNone -> 2
        FieldLogLevel' name -> (error . showText) $ "Unknown FieldLogLevel: " <> original name

-- | Represents the bounds of /known/ $FieldLogLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FieldLogLevel where
    minBound = FLLAll
    maxBound = FLLNone

instance Hashable     FieldLogLevel
instance NFData       FieldLogLevel
instance ToByteString FieldLogLevel
instance ToQuery      FieldLogLevel
instance ToHeader     FieldLogLevel

instance ToJSON FieldLogLevel where
    toJSON = toJSONText

instance FromJSON FieldLogLevel where
    parseJSON = parseJSONText "FieldLogLevel"
