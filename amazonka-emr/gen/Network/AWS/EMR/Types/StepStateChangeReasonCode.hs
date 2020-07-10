{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepStateChangeReasonCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.StepStateChangeReasonCode (
  StepStateChangeReasonCode (
    ..
    , SSCRCNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StepStateChangeReasonCode = StepStateChangeReasonCode' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern SSCRCNone :: StepStateChangeReasonCode
pattern SSCRCNone = StepStateChangeReasonCode' "NONE"

{-# COMPLETE
  SSCRCNone,
  StepStateChangeReasonCode' #-}

instance FromText StepStateChangeReasonCode where
    parser = (StepStateChangeReasonCode' . mk) <$> takeText

instance ToText StepStateChangeReasonCode where
    toText (StepStateChangeReasonCode' ci) = original ci

-- | Represents an enum of /known/ $StepStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StepStateChangeReasonCode where
    toEnum i = case i of
        0 -> SSCRCNone
        _ -> (error . showText) $ "Unknown index for StepStateChangeReasonCode: " <> toText i
    fromEnum x = case x of
        SSCRCNone -> 0
        StepStateChangeReasonCode' name -> (error . showText) $ "Unknown StepStateChangeReasonCode: " <> original name

-- | Represents the bounds of /known/ $StepStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StepStateChangeReasonCode where
    minBound = SSCRCNone
    maxBound = SSCRCNone

instance Hashable     StepStateChangeReasonCode
instance NFData       StepStateChangeReasonCode
instance ToByteString StepStateChangeReasonCode
instance ToQuery      StepStateChangeReasonCode
instance ToHeader     StepStateChangeReasonCode

instance FromJSON StepStateChangeReasonCode where
    parseJSON = parseJSONText "StepStateChangeReasonCode"
