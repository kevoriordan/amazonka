{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.FileSourceConvert608To708
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.FileSourceConvert608To708 (
  FileSourceConvert608To708 (
    ..
    , FSCTDisabled
    , FSCTUpconvert
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
data FileSourceConvert608To708 = FileSourceConvert608To708' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern FSCTDisabled :: FileSourceConvert608To708
pattern FSCTDisabled = FileSourceConvert608To708' "DISABLED"

pattern FSCTUpconvert :: FileSourceConvert608To708
pattern FSCTUpconvert = FileSourceConvert608To708' "UPCONVERT"

{-# COMPLETE
  FSCTDisabled,
  FSCTUpconvert,
  FileSourceConvert608To708' #-}

instance FromText FileSourceConvert608To708 where
    parser = (FileSourceConvert608To708' . mk) <$> takeText

instance ToText FileSourceConvert608To708 where
    toText (FileSourceConvert608To708' ci) = original ci

-- | Represents an enum of /known/ $FileSourceConvert608To708.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FileSourceConvert608To708 where
    toEnum i = case i of
        0 -> FSCTDisabled
        1 -> FSCTUpconvert
        _ -> (error . showText) $ "Unknown index for FileSourceConvert608To708: " <> toText i
    fromEnum x = case x of
        FSCTDisabled -> 0
        FSCTUpconvert -> 1
        FileSourceConvert608To708' name -> (error . showText) $ "Unknown FileSourceConvert608To708: " <> original name

-- | Represents the bounds of /known/ $FileSourceConvert608To708.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FileSourceConvert608To708 where
    minBound = FSCTDisabled
    maxBound = FSCTUpconvert

instance Hashable     FileSourceConvert608To708
instance NFData       FileSourceConvert608To708
instance ToByteString FileSourceConvert608To708
instance ToQuery      FileSourceConvert608To708
instance ToHeader     FileSourceConvert608To708

instance ToJSON FileSourceConvert608To708 where
    toJSON = toJSONText

instance FromJSON FileSourceConvert608To708 where
    parseJSON = parseJSONText "FileSourceConvert608To708"
