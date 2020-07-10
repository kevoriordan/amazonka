{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.LifecycleErrorCode (
  LifecycleErrorCode (
    ..
    , ScriptFailed
    , ScriptMissing
    , ScriptNotExecutable
    , ScriptTimedOut
    , Success
    , UnknownError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LifecycleErrorCode = LifecycleErrorCode' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern ScriptFailed :: LifecycleErrorCode
pattern ScriptFailed = LifecycleErrorCode' "ScriptFailed"

pattern ScriptMissing :: LifecycleErrorCode
pattern ScriptMissing = LifecycleErrorCode' "ScriptMissing"

pattern ScriptNotExecutable :: LifecycleErrorCode
pattern ScriptNotExecutable = LifecycleErrorCode' "ScriptNotExecutable"

pattern ScriptTimedOut :: LifecycleErrorCode
pattern ScriptTimedOut = LifecycleErrorCode' "ScriptTimedOut"

pattern Success :: LifecycleErrorCode
pattern Success = LifecycleErrorCode' "Success"

pattern UnknownError :: LifecycleErrorCode
pattern UnknownError = LifecycleErrorCode' "UnknownError"

{-# COMPLETE
  ScriptFailed,
  ScriptMissing,
  ScriptNotExecutable,
  ScriptTimedOut,
  Success,
  UnknownError,
  LifecycleErrorCode' #-}

instance FromText LifecycleErrorCode where
    parser = (LifecycleErrorCode' . mk) <$> takeText

instance ToText LifecycleErrorCode where
    toText (LifecycleErrorCode' ci) = original ci

-- | Represents an enum of /known/ $LifecycleErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LifecycleErrorCode where
    toEnum i = case i of
        0 -> ScriptFailed
        1 -> ScriptMissing
        2 -> ScriptNotExecutable
        3 -> ScriptTimedOut
        4 -> Success
        5 -> UnknownError
        _ -> (error . showText) $ "Unknown index for LifecycleErrorCode: " <> toText i
    fromEnum x = case x of
        ScriptFailed -> 0
        ScriptMissing -> 1
        ScriptNotExecutable -> 2
        ScriptTimedOut -> 3
        Success -> 4
        UnknownError -> 5
        LifecycleErrorCode' name -> (error . showText) $ "Unknown LifecycleErrorCode: " <> original name

-- | Represents the bounds of /known/ $LifecycleErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LifecycleErrorCode where
    minBound = ScriptFailed
    maxBound = UnknownError

instance Hashable     LifecycleErrorCode
instance NFData       LifecycleErrorCode
instance ToByteString LifecycleErrorCode
instance ToQuery      LifecycleErrorCode
instance ToHeader     LifecycleErrorCode

instance FromJSON LifecycleErrorCode where
    parseJSON = parseJSONText "LifecycleErrorCode"
