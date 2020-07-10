{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CompilationJobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CompilationJobStatus (
  CompilationJobStatus (
    ..
    , CJSCompleted
    , CJSFailed
    , CJSInprogress
    , CJSStarting
    , CJSStopped
    , CJSStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CompilationJobStatus = CompilationJobStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern CJSCompleted :: CompilationJobStatus
pattern CJSCompleted = CompilationJobStatus' "COMPLETED"

pattern CJSFailed :: CompilationJobStatus
pattern CJSFailed = CompilationJobStatus' "FAILED"

pattern CJSInprogress :: CompilationJobStatus
pattern CJSInprogress = CompilationJobStatus' "INPROGRESS"

pattern CJSStarting :: CompilationJobStatus
pattern CJSStarting = CompilationJobStatus' "STARTING"

pattern CJSStopped :: CompilationJobStatus
pattern CJSStopped = CompilationJobStatus' "STOPPED"

pattern CJSStopping :: CompilationJobStatus
pattern CJSStopping = CompilationJobStatus' "STOPPING"

{-# COMPLETE
  CJSCompleted,
  CJSFailed,
  CJSInprogress,
  CJSStarting,
  CJSStopped,
  CJSStopping,
  CompilationJobStatus' #-}

instance FromText CompilationJobStatus where
    parser = (CompilationJobStatus' . mk) <$> takeText

instance ToText CompilationJobStatus where
    toText (CompilationJobStatus' ci) = original ci

-- | Represents an enum of /known/ $CompilationJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CompilationJobStatus where
    toEnum i = case i of
        0 -> CJSCompleted
        1 -> CJSFailed
        2 -> CJSInprogress
        3 -> CJSStarting
        4 -> CJSStopped
        5 -> CJSStopping
        _ -> (error . showText) $ "Unknown index for CompilationJobStatus: " <> toText i
    fromEnum x = case x of
        CJSCompleted -> 0
        CJSFailed -> 1
        CJSInprogress -> 2
        CJSStarting -> 3
        CJSStopped -> 4
        CJSStopping -> 5
        CompilationJobStatus' name -> (error . showText) $ "Unknown CompilationJobStatus: " <> original name

-- | Represents the bounds of /known/ $CompilationJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CompilationJobStatus where
    minBound = CJSCompleted
    maxBound = CJSStopping

instance Hashable     CompilationJobStatus
instance NFData       CompilationJobStatus
instance ToByteString CompilationJobStatus
instance ToQuery      CompilationJobStatus
instance ToHeader     CompilationJobStatus

instance ToJSON CompilationJobStatus where
    toJSON = toJSONText

instance FromJSON CompilationJobStatus where
    parseJSON = parseJSONText "CompilationJobStatus"
