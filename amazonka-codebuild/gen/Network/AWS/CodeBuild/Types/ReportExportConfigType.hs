{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportExportConfigType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ReportExportConfigType (
  ReportExportConfigType (
    ..
    , RECTNoExport
    , RECTS3
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReportExportConfigType = ReportExportConfigType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern RECTNoExport :: ReportExportConfigType
pattern RECTNoExport = ReportExportConfigType' "NO_EXPORT"

pattern RECTS3 :: ReportExportConfigType
pattern RECTS3 = ReportExportConfigType' "S3"

{-# COMPLETE
  RECTNoExport,
  RECTS3,
  ReportExportConfigType' #-}

instance FromText ReportExportConfigType where
    parser = (ReportExportConfigType' . mk) <$> takeText

instance ToText ReportExportConfigType where
    toText (ReportExportConfigType' ci) = original ci

-- | Represents an enum of /known/ $ReportExportConfigType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReportExportConfigType where
    toEnum i = case i of
        0 -> RECTNoExport
        1 -> RECTS3
        _ -> (error . showText) $ "Unknown index for ReportExportConfigType: " <> toText i
    fromEnum x = case x of
        RECTNoExport -> 0
        RECTS3 -> 1
        ReportExportConfigType' name -> (error . showText) $ "Unknown ReportExportConfigType: " <> original name

-- | Represents the bounds of /known/ $ReportExportConfigType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReportExportConfigType where
    minBound = RECTNoExport
    maxBound = RECTS3

instance Hashable     ReportExportConfigType
instance NFData       ReportExportConfigType
instance ToByteString ReportExportConfigType
instance ToQuery      ReportExportConfigType
instance ToHeader     ReportExportConfigType

instance ToJSON ReportExportConfigType where
    toJSON = toJSONText

instance FromJSON ReportExportConfigType where
    parseJSON = parseJSONText "ReportExportConfigType"
