{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber (
  DASHDisplayFragmentNumber (
    ..
    , DASHDFNAlways
    , DASHDFNNever
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DASHDisplayFragmentNumber = DASHDisplayFragmentNumber' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern DASHDFNAlways :: DASHDisplayFragmentNumber
pattern DASHDFNAlways = DASHDisplayFragmentNumber' "ALWAYS"

pattern DASHDFNNever :: DASHDisplayFragmentNumber
pattern DASHDFNNever = DASHDisplayFragmentNumber' "NEVER"

{-# COMPLETE
  DASHDFNAlways,
  DASHDFNNever,
  DASHDisplayFragmentNumber' #-}

instance FromText DASHDisplayFragmentNumber where
    parser = (DASHDisplayFragmentNumber' . mk) <$> takeText

instance ToText DASHDisplayFragmentNumber where
    toText (DASHDisplayFragmentNumber' ci) = original ci

-- | Represents an enum of /known/ $DASHDisplayFragmentNumber.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DASHDisplayFragmentNumber where
    toEnum i = case i of
        0 -> DASHDFNAlways
        1 -> DASHDFNNever
        _ -> (error . showText) $ "Unknown index for DASHDisplayFragmentNumber: " <> toText i
    fromEnum x = case x of
        DASHDFNAlways -> 0
        DASHDFNNever -> 1
        DASHDisplayFragmentNumber' name -> (error . showText) $ "Unknown DASHDisplayFragmentNumber: " <> original name

-- | Represents the bounds of /known/ $DASHDisplayFragmentNumber.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DASHDisplayFragmentNumber where
    minBound = DASHDFNAlways
    maxBound = DASHDFNNever

instance Hashable     DASHDisplayFragmentNumber
instance NFData       DASHDisplayFragmentNumber
instance ToByteString DASHDisplayFragmentNumber
instance ToQuery      DASHDisplayFragmentNumber
instance ToHeader     DASHDisplayFragmentNumber

instance ToJSON DASHDisplayFragmentNumber where
    toJSON = toJSONText
