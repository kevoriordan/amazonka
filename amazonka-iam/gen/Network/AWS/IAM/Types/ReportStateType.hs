{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ReportStateType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.ReportStateType (
  ReportStateType (
    ..
    , RSTComplete
    , RSTInprogress
    , RSTStarted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReportStateType = ReportStateType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern RSTComplete :: ReportStateType
pattern RSTComplete = ReportStateType' "COMPLETE"

pattern RSTInprogress :: ReportStateType
pattern RSTInprogress = ReportStateType' "INPROGRESS"

pattern RSTStarted :: ReportStateType
pattern RSTStarted = ReportStateType' "STARTED"

{-# COMPLETE
  RSTComplete,
  RSTInprogress,
  RSTStarted,
  ReportStateType' #-}

instance FromText ReportStateType where
    parser = (ReportStateType' . mk) <$> takeText

instance ToText ReportStateType where
    toText (ReportStateType' ci) = original ci

-- | Represents an enum of /known/ $ReportStateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReportStateType where
    toEnum i = case i of
        0 -> RSTComplete
        1 -> RSTInprogress
        2 -> RSTStarted
        _ -> (error . showText) $ "Unknown index for ReportStateType: " <> toText i
    fromEnum x = case x of
        RSTComplete -> 0
        RSTInprogress -> 1
        RSTStarted -> 2
        ReportStateType' name -> (error . showText) $ "Unknown ReportStateType: " <> original name

-- | Represents the bounds of /known/ $ReportStateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReportStateType where
    minBound = RSTComplete
    maxBound = RSTStarted

instance Hashable     ReportStateType
instance NFData       ReportStateType
instance ToByteString ReportStateType
instance ToQuery      ReportStateType
instance ToHeader     ReportStateType

instance FromXML ReportStateType where
    parseXML = parseXMLText "ReportStateType"
