{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CancelTimerFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.CancelTimerFailedCause (
  CancelTimerFailedCause (
    ..
    , CTFCOperationNotPermitted
    , CTFCTimerIdUnknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CancelTimerFailedCause = CancelTimerFailedCause' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern CTFCOperationNotPermitted :: CancelTimerFailedCause
pattern CTFCOperationNotPermitted = CancelTimerFailedCause' "OPERATION_NOT_PERMITTED"

pattern CTFCTimerIdUnknown :: CancelTimerFailedCause
pattern CTFCTimerIdUnknown = CancelTimerFailedCause' "TIMER_ID_UNKNOWN"

{-# COMPLETE
  CTFCOperationNotPermitted,
  CTFCTimerIdUnknown,
  CancelTimerFailedCause' #-}

instance FromText CancelTimerFailedCause where
    parser = (CancelTimerFailedCause' . mk) <$> takeText

instance ToText CancelTimerFailedCause where
    toText (CancelTimerFailedCause' ci) = original ci

-- | Represents an enum of /known/ $CancelTimerFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CancelTimerFailedCause where
    toEnum i = case i of
        0 -> CTFCOperationNotPermitted
        1 -> CTFCTimerIdUnknown
        _ -> (error . showText) $ "Unknown index for CancelTimerFailedCause: " <> toText i
    fromEnum x = case x of
        CTFCOperationNotPermitted -> 0
        CTFCTimerIdUnknown -> 1
        CancelTimerFailedCause' name -> (error . showText) $ "Unknown CancelTimerFailedCause: " <> original name

-- | Represents the bounds of /known/ $CancelTimerFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CancelTimerFailedCause where
    minBound = CTFCOperationNotPermitted
    maxBound = CTFCTimerIdUnknown

instance Hashable     CancelTimerFailedCause
instance NFData       CancelTimerFailedCause
instance ToByteString CancelTimerFailedCause
instance ToQuery      CancelTimerFailedCause
instance ToHeader     CancelTimerFailedCause

instance FromJSON CancelTimerFailedCause where
    parseJSON = parseJSONText "CancelTimerFailedCause"
