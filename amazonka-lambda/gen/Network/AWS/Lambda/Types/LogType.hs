{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LogType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.LogType (
  LogType (
    ..
    , None
    , Tail
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LogType = LogType' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern None :: LogType
pattern None = LogType' "None"

pattern Tail :: LogType
pattern Tail = LogType' "Tail"

{-# COMPLETE
  None,
  Tail,
  LogType' #-}

instance FromText LogType where
    parser = (LogType' . mk) <$> takeText

instance ToText LogType where
    toText (LogType' ci) = original ci

-- | Represents an enum of /known/ $LogType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LogType where
    toEnum i = case i of
        0 -> None
        1 -> Tail
        _ -> (error . showText) $ "Unknown index for LogType: " <> toText i
    fromEnum x = case x of
        None -> 0
        Tail -> 1
        LogType' name -> (error . showText) $ "Unknown LogType: " <> original name

-- | Represents the bounds of /known/ $LogType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LogType where
    minBound = None
    maxBound = Tail

instance Hashable     LogType
instance NFData       LogType
instance ToByteString LogType
instance ToQuery      LogType
instance ToHeader     LogType

instance ToJSON LogType where
    toJSON = toJSONText
