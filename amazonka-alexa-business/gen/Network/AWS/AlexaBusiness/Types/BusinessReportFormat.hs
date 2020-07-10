{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.BusinessReportFormat (
  BusinessReportFormat (
    ..
    , CSV
    , CSVZip
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BusinessReportFormat = BusinessReportFormat' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern CSV :: BusinessReportFormat
pattern CSV = BusinessReportFormat' "CSV"

pattern CSVZip :: BusinessReportFormat
pattern CSVZip = BusinessReportFormat' "CSV_ZIP"

{-# COMPLETE
  CSV,
  CSVZip,
  BusinessReportFormat' #-}

instance FromText BusinessReportFormat where
    parser = (BusinessReportFormat' . mk) <$> takeText

instance ToText BusinessReportFormat where
    toText (BusinessReportFormat' ci) = original ci

-- | Represents an enum of /known/ $BusinessReportFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BusinessReportFormat where
    toEnum i = case i of
        0 -> CSV
        1 -> CSVZip
        _ -> (error . showText) $ "Unknown index for BusinessReportFormat: " <> toText i
    fromEnum x = case x of
        CSV -> 0
        CSVZip -> 1
        BusinessReportFormat' name -> (error . showText) $ "Unknown BusinessReportFormat: " <> original name

-- | Represents the bounds of /known/ $BusinessReportFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BusinessReportFormat where
    minBound = CSV
    maxBound = CSVZip

instance Hashable     BusinessReportFormat
instance NFData       BusinessReportFormat
instance ToByteString BusinessReportFormat
instance ToQuery      BusinessReportFormat
instance ToHeader     BusinessReportFormat

instance ToJSON BusinessReportFormat where
    toJSON = toJSONText

instance FromJSON BusinessReportFormat where
    parseJSON = parseJSONText "BusinessReportFormat"
