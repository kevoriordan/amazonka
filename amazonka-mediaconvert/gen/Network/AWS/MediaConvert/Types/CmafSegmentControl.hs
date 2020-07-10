{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafSegmentControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafSegmentControl (
  CmafSegmentControl (
    ..
    , SegmentedFiles
    , SingleFile
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
data CmafSegmentControl = CmafSegmentControl' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern SegmentedFiles :: CmafSegmentControl
pattern SegmentedFiles = CmafSegmentControl' "SEGMENTED_FILES"

pattern SingleFile :: CmafSegmentControl
pattern SingleFile = CmafSegmentControl' "SINGLE_FILE"

{-# COMPLETE
  SegmentedFiles,
  SingleFile,
  CmafSegmentControl' #-}

instance FromText CmafSegmentControl where
    parser = (CmafSegmentControl' . mk) <$> takeText

instance ToText CmafSegmentControl where
    toText (CmafSegmentControl' ci) = original ci

-- | Represents an enum of /known/ $CmafSegmentControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmafSegmentControl where
    toEnum i = case i of
        0 -> SegmentedFiles
        1 -> SingleFile
        _ -> (error . showText) $ "Unknown index for CmafSegmentControl: " <> toText i
    fromEnum x = case x of
        SegmentedFiles -> 0
        SingleFile -> 1
        CmafSegmentControl' name -> (error . showText) $ "Unknown CmafSegmentControl: " <> original name

-- | Represents the bounds of /known/ $CmafSegmentControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmafSegmentControl where
    minBound = SegmentedFiles
    maxBound = SingleFile

instance Hashable     CmafSegmentControl
instance NFData       CmafSegmentControl
instance ToByteString CmafSegmentControl
instance ToQuery      CmafSegmentControl
instance ToHeader     CmafSegmentControl

instance ToJSON CmafSegmentControl where
    toJSON = toJSONText

instance FromJSON CmafSegmentControl where
    parseJSON = parseJSONText "CmafSegmentControl"
