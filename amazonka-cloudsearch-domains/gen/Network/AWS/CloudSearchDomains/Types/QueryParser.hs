{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.QueryParser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types.QueryParser (
  QueryParser (
    ..
    , Dismax
    , Lucene
    , Simple
    , Structured
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data QueryParser = QueryParser' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Dismax :: QueryParser
pattern Dismax = QueryParser' "dismax"

pattern Lucene :: QueryParser
pattern Lucene = QueryParser' "lucene"

pattern Simple :: QueryParser
pattern Simple = QueryParser' "simple"

pattern Structured :: QueryParser
pattern Structured = QueryParser' "structured"

{-# COMPLETE
  Dismax,
  Lucene,
  Simple,
  Structured,
  QueryParser' #-}

instance FromText QueryParser where
    parser = (QueryParser' . mk) <$> takeText

instance ToText QueryParser where
    toText (QueryParser' ci) = original ci

-- | Represents an enum of /known/ $QueryParser.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum QueryParser where
    toEnum i = case i of
        0 -> Dismax
        1 -> Lucene
        2 -> Simple
        3 -> Structured
        _ -> (error . showText) $ "Unknown index for QueryParser: " <> toText i
    fromEnum x = case x of
        Dismax -> 0
        Lucene -> 1
        Simple -> 2
        Structured -> 3
        QueryParser' name -> (error . showText) $ "Unknown QueryParser: " <> original name

-- | Represents the bounds of /known/ $QueryParser.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded QueryParser where
    minBound = Dismax
    maxBound = Structured

instance Hashable     QueryParser
instance NFData       QueryParser
instance ToByteString QueryParser
instance ToQuery      QueryParser
instance ToHeader     QueryParser

instance ToJSON QueryParser where
    toJSON = toJSONText
