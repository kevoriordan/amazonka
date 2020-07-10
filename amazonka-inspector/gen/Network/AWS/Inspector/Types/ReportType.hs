{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ReportType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.ReportType (
  ReportType (
    ..
    , Finding
    , Full
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReportType = ReportType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Finding :: ReportType
pattern Finding = ReportType' "FINDING"

pattern Full :: ReportType
pattern Full = ReportType' "FULL"

{-# COMPLETE
  Finding,
  Full,
  ReportType' #-}

instance FromText ReportType where
    parser = (ReportType' . mk) <$> takeText

instance ToText ReportType where
    toText (ReportType' ci) = original ci

-- | Represents an enum of /known/ $ReportType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReportType where
    toEnum i = case i of
        0 -> Finding
        1 -> Full
        _ -> (error . showText) $ "Unknown index for ReportType: " <> toText i
    fromEnum x = case x of
        Finding -> 0
        Full -> 1
        ReportType' name -> (error . showText) $ "Unknown ReportType: " <> original name

-- | Represents the bounds of /known/ $ReportType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReportType where
    minBound = Finding
    maxBound = Full

instance Hashable     ReportType
instance NFData       ReportType
instance ToByteString ReportType
instance ToQuery      ReportType
instance ToHeader     ReportType

instance ToJSON ReportType where
    toJSON = toJSONText
