{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CaptureStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CaptureStatus (
  CaptureStatus (
    ..
    , CStarted
    , CStopped
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CaptureStatus = CaptureStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern CStarted :: CaptureStatus
pattern CStarted = CaptureStatus' "Started"

pattern CStopped :: CaptureStatus
pattern CStopped = CaptureStatus' "Stopped"

{-# COMPLETE
  CStarted,
  CStopped,
  CaptureStatus' #-}

instance FromText CaptureStatus where
    parser = (CaptureStatus' . mk) <$> takeText

instance ToText CaptureStatus where
    toText (CaptureStatus' ci) = original ci

-- | Represents an enum of /known/ $CaptureStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CaptureStatus where
    toEnum i = case i of
        0 -> CStarted
        1 -> CStopped
        _ -> (error . showText) $ "Unknown index for CaptureStatus: " <> toText i
    fromEnum x = case x of
        CStarted -> 0
        CStopped -> 1
        CaptureStatus' name -> (error . showText) $ "Unknown CaptureStatus: " <> original name

-- | Represents the bounds of /known/ $CaptureStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CaptureStatus where
    minBound = CStarted
    maxBound = CStopped

instance Hashable     CaptureStatus
instance NFData       CaptureStatus
instance ToByteString CaptureStatus
instance ToQuery      CaptureStatus
instance ToHeader     CaptureStatus

instance FromJSON CaptureStatus where
    parseJSON = parseJSONText "CaptureStatus"
