{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LogicalOperator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.LogicalOperator (
  LogicalOperator (
    ..
    , Equals
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LogicalOperator = LogicalOperator' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Equals :: LogicalOperator
pattern Equals = LogicalOperator' "EQUALS"

{-# COMPLETE
  Equals,
  LogicalOperator' #-}

instance FromText LogicalOperator where
    parser = (LogicalOperator' . mk) <$> takeText

instance ToText LogicalOperator where
    toText (LogicalOperator' ci) = original ci

-- | Represents an enum of /known/ $LogicalOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LogicalOperator where
    toEnum i = case i of
        0 -> Equals
        _ -> (error . showText) $ "Unknown index for LogicalOperator: " <> toText i
    fromEnum x = case x of
        Equals -> 0
        LogicalOperator' name -> (error . showText) $ "Unknown LogicalOperator: " <> original name

-- | Represents the bounds of /known/ $LogicalOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LogicalOperator where
    minBound = Equals
    maxBound = Equals

instance Hashable     LogicalOperator
instance NFData       LogicalOperator
instance ToByteString LogicalOperator
instance ToQuery      LogicalOperator
instance ToHeader     LogicalOperator

instance ToJSON LogicalOperator where
    toJSON = toJSONText

instance FromJSON LogicalOperator where
    parseJSON = parseJSONText "LogicalOperator"
