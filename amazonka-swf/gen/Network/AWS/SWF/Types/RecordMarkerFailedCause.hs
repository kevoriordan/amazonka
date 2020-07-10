{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RecordMarkerFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.RecordMarkerFailedCause (
  RecordMarkerFailedCause (
    ..
    , OperationNotPermitted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RecordMarkerFailedCause = RecordMarkerFailedCause' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern OperationNotPermitted :: RecordMarkerFailedCause
pattern OperationNotPermitted = RecordMarkerFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE
  OperationNotPermitted,
  RecordMarkerFailedCause' #-}

instance FromText RecordMarkerFailedCause where
    parser = (RecordMarkerFailedCause' . mk) <$> takeText

instance ToText RecordMarkerFailedCause where
    toText (RecordMarkerFailedCause' ci) = original ci

-- | Represents an enum of /known/ $RecordMarkerFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RecordMarkerFailedCause where
    toEnum i = case i of
        0 -> OperationNotPermitted
        _ -> (error . showText) $ "Unknown index for RecordMarkerFailedCause: " <> toText i
    fromEnum x = case x of
        OperationNotPermitted -> 0
        RecordMarkerFailedCause' name -> (error . showText) $ "Unknown RecordMarkerFailedCause: " <> original name

-- | Represents the bounds of /known/ $RecordMarkerFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RecordMarkerFailedCause where
    minBound = OperationNotPermitted
    maxBound = OperationNotPermitted

instance Hashable     RecordMarkerFailedCause
instance NFData       RecordMarkerFailedCause
instance ToByteString RecordMarkerFailedCause
instance ToQuery      RecordMarkerFailedCause
instance ToHeader     RecordMarkerFailedCause

instance FromJSON RecordMarkerFailedCause where
    parseJSON = parseJSONText "RecordMarkerFailedCause"
