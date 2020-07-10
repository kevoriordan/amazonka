{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetadataDirective
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.MetadataDirective (
  MetadataDirective (
    ..
    , MDCopy
    , MDReplace
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data MetadataDirective = MetadataDirective' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern MDCopy :: MetadataDirective
pattern MDCopy = MetadataDirective' "COPY"

pattern MDReplace :: MetadataDirective
pattern MDReplace = MetadataDirective' "REPLACE"

{-# COMPLETE
  MDCopy,
  MDReplace,
  MetadataDirective' #-}

instance FromText MetadataDirective where
    parser = (MetadataDirective' . mk) <$> takeText

instance ToText MetadataDirective where
    toText (MetadataDirective' ci) = original ci

-- | Represents an enum of /known/ $MetadataDirective.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MetadataDirective where
    toEnum i = case i of
        0 -> MDCopy
        1 -> MDReplace
        _ -> (error . showText) $ "Unknown index for MetadataDirective: " <> toText i
    fromEnum x = case x of
        MDCopy -> 0
        MDReplace -> 1
        MetadataDirective' name -> (error . showText) $ "Unknown MetadataDirective: " <> original name

-- | Represents the bounds of /known/ $MetadataDirective.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MetadataDirective where
    minBound = MDCopy
    maxBound = MDReplace

instance Hashable     MetadataDirective
instance NFData       MetadataDirective
instance ToByteString MetadataDirective
instance ToQuery      MetadataDirective
instance ToHeader     MetadataDirective

instance ToXML MetadataDirective where
    toXML = toXMLText
