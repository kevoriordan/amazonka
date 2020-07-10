{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.InputFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.InputFormat (
  InputFormat (
    ..
    , OneDocPerFile
    , OneDocPerLine
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InputFormat = InputFormat' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern OneDocPerFile :: InputFormat
pattern OneDocPerFile = InputFormat' "ONE_DOC_PER_FILE"

pattern OneDocPerLine :: InputFormat
pattern OneDocPerLine = InputFormat' "ONE_DOC_PER_LINE"

{-# COMPLETE
  OneDocPerFile,
  OneDocPerLine,
  InputFormat' #-}

instance FromText InputFormat where
    parser = (InputFormat' . mk) <$> takeText

instance ToText InputFormat where
    toText (InputFormat' ci) = original ci

-- | Represents an enum of /known/ $InputFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputFormat where
    toEnum i = case i of
        0 -> OneDocPerFile
        1 -> OneDocPerLine
        _ -> (error . showText) $ "Unknown index for InputFormat: " <> toText i
    fromEnum x = case x of
        OneDocPerFile -> 0
        OneDocPerLine -> 1
        InputFormat' name -> (error . showText) $ "Unknown InputFormat: " <> original name

-- | Represents the bounds of /known/ $InputFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputFormat where
    minBound = OneDocPerFile
    maxBound = OneDocPerLine

instance Hashable     InputFormat
instance NFData       InputFormat
instance ToByteString InputFormat
instance ToQuery      InputFormat
instance ToHeader     InputFormat

instance ToJSON InputFormat where
    toJSON = toJSONText

instance FromJSON InputFormat where
    parseJSON = parseJSONText "InputFormat"
