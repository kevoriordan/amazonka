{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.PreviewStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.PreviewStatus (
  PreviewStatus (
    ..
    , PSCompleted
    , PSWorkInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PreviewStatus = PreviewStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern PSCompleted :: PreviewStatus
pattern PSCompleted = PreviewStatus' "COMPLETED"

pattern PSWorkInProgress :: PreviewStatus
pattern PSWorkInProgress = PreviewStatus' "WORK_IN_PROGRESS"

{-# COMPLETE
  PSCompleted,
  PSWorkInProgress,
  PreviewStatus' #-}

instance FromText PreviewStatus where
    parser = (PreviewStatus' . mk) <$> takeText

instance ToText PreviewStatus where
    toText (PreviewStatus' ci) = original ci

-- | Represents an enum of /known/ $PreviewStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PreviewStatus where
    toEnum i = case i of
        0 -> PSCompleted
        1 -> PSWorkInProgress
        _ -> (error . showText) $ "Unknown index for PreviewStatus: " <> toText i
    fromEnum x = case x of
        PSCompleted -> 0
        PSWorkInProgress -> 1
        PreviewStatus' name -> (error . showText) $ "Unknown PreviewStatus: " <> original name

-- | Represents the bounds of /known/ $PreviewStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PreviewStatus where
    minBound = PSCompleted
    maxBound = PSWorkInProgress

instance Hashable     PreviewStatus
instance NFData       PreviewStatus
instance ToByteString PreviewStatus
instance ToQuery      PreviewStatus
instance ToHeader     PreviewStatus

instance FromJSON PreviewStatus where
    parseJSON = parseJSONText "PreviewStatus"
