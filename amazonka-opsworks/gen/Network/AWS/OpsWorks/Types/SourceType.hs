{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.SourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.SourceType (
  SourceType (
    ..
    , Archive
    , Git
    , S3
    , SVN
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SourceType = SourceType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Archive :: SourceType
pattern Archive = SourceType' "archive"

pattern Git :: SourceType
pattern Git = SourceType' "git"

pattern S3 :: SourceType
pattern S3 = SourceType' "s3"

pattern SVN :: SourceType
pattern SVN = SourceType' "svn"

{-# COMPLETE
  Archive,
  Git,
  S3,
  SVN,
  SourceType' #-}

instance FromText SourceType where
    parser = (SourceType' . mk) <$> takeText

instance ToText SourceType where
    toText (SourceType' ci) = original ci

-- | Represents an enum of /known/ $SourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SourceType where
    toEnum i = case i of
        0 -> Archive
        1 -> Git
        2 -> S3
        3 -> SVN
        _ -> (error . showText) $ "Unknown index for SourceType: " <> toText i
    fromEnum x = case x of
        Archive -> 0
        Git -> 1
        S3 -> 2
        SVN -> 3
        SourceType' name -> (error . showText) $ "Unknown SourceType: " <> original name

-- | Represents the bounds of /known/ $SourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SourceType where
    minBound = Archive
    maxBound = SVN

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance ToJSON SourceType where
    toJSON = toJSONText

instance FromJSON SourceType where
    parseJSON = parseJSONText "SourceType"
