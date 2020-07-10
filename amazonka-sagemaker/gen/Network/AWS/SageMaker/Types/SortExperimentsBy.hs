{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SortExperimentsBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SortExperimentsBy (
  SortExperimentsBy (
    ..
    , SEBCreationTime
    , SEBName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SortExperimentsBy = SortExperimentsBy' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern SEBCreationTime :: SortExperimentsBy
pattern SEBCreationTime = SortExperimentsBy' "CreationTime"

pattern SEBName :: SortExperimentsBy
pattern SEBName = SortExperimentsBy' "Name"

{-# COMPLETE
  SEBCreationTime,
  SEBName,
  SortExperimentsBy' #-}

instance FromText SortExperimentsBy where
    parser = (SortExperimentsBy' . mk) <$> takeText

instance ToText SortExperimentsBy where
    toText (SortExperimentsBy' ci) = original ci

-- | Represents an enum of /known/ $SortExperimentsBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SortExperimentsBy where
    toEnum i = case i of
        0 -> SEBCreationTime
        1 -> SEBName
        _ -> (error . showText) $ "Unknown index for SortExperimentsBy: " <> toText i
    fromEnum x = case x of
        SEBCreationTime -> 0
        SEBName -> 1
        SortExperimentsBy' name -> (error . showText) $ "Unknown SortExperimentsBy: " <> original name

-- | Represents the bounds of /known/ $SortExperimentsBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortExperimentsBy where
    minBound = SEBCreationTime
    maxBound = SEBName

instance Hashable     SortExperimentsBy
instance NFData       SortExperimentsBy
instance ToByteString SortExperimentsBy
instance ToQuery      SortExperimentsBy
instance ToHeader     SortExperimentsBy

instance ToJSON SortExperimentsBy where
    toJSON = toJSONText
