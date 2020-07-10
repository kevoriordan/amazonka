{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentTimestamp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentTimestamp (
  DASHDisplayFragmentTimestamp (
    ..
    , DASHDFTAlways
    , DASHDFTNever
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DASHDisplayFragmentTimestamp = DASHDisplayFragmentTimestamp' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern DASHDFTAlways :: DASHDisplayFragmentTimestamp
pattern DASHDFTAlways = DASHDisplayFragmentTimestamp' "ALWAYS"

pattern DASHDFTNever :: DASHDisplayFragmentTimestamp
pattern DASHDFTNever = DASHDisplayFragmentTimestamp' "NEVER"

{-# COMPLETE
  DASHDFTAlways,
  DASHDFTNever,
  DASHDisplayFragmentTimestamp' #-}

instance FromText DASHDisplayFragmentTimestamp where
    parser = (DASHDisplayFragmentTimestamp' . mk) <$> takeText

instance ToText DASHDisplayFragmentTimestamp where
    toText (DASHDisplayFragmentTimestamp' ci) = original ci

-- | Represents an enum of /known/ $DASHDisplayFragmentTimestamp.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DASHDisplayFragmentTimestamp where
    toEnum i = case i of
        0 -> DASHDFTAlways
        1 -> DASHDFTNever
        _ -> (error . showText) $ "Unknown index for DASHDisplayFragmentTimestamp: " <> toText i
    fromEnum x = case x of
        DASHDFTAlways -> 0
        DASHDFTNever -> 1
        DASHDisplayFragmentTimestamp' name -> (error . showText) $ "Unknown DASHDisplayFragmentTimestamp: " <> original name

-- | Represents the bounds of /known/ $DASHDisplayFragmentTimestamp.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DASHDisplayFragmentTimestamp where
    minBound = DASHDFTAlways
    maxBound = DASHDFTNever

instance Hashable     DASHDisplayFragmentTimestamp
instance NFData       DASHDisplayFragmentTimestamp
instance ToByteString DASHDisplayFragmentTimestamp
instance ToQuery      DASHDisplayFragmentTimestamp
instance ToHeader     DASHDisplayFragmentTimestamp

instance ToJSON DASHDisplayFragmentTimestamp where
    toJSON = toJSONText
