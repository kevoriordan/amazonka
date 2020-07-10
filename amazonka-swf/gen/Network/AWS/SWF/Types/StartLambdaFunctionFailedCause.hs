{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartLambdaFunctionFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.StartLambdaFunctionFailedCause (
  StartLambdaFunctionFailedCause (
    ..
    , AssumeRoleFailed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StartLambdaFunctionFailedCause = StartLambdaFunctionFailedCause' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern AssumeRoleFailed :: StartLambdaFunctionFailedCause
pattern AssumeRoleFailed = StartLambdaFunctionFailedCause' "ASSUME_ROLE_FAILED"

{-# COMPLETE
  AssumeRoleFailed,
  StartLambdaFunctionFailedCause' #-}

instance FromText StartLambdaFunctionFailedCause where
    parser = (StartLambdaFunctionFailedCause' . mk) <$> takeText

instance ToText StartLambdaFunctionFailedCause where
    toText (StartLambdaFunctionFailedCause' ci) = original ci

-- | Represents an enum of /known/ $StartLambdaFunctionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StartLambdaFunctionFailedCause where
    toEnum i = case i of
        0 -> AssumeRoleFailed
        _ -> (error . showText) $ "Unknown index for StartLambdaFunctionFailedCause: " <> toText i
    fromEnum x = case x of
        AssumeRoleFailed -> 0
        StartLambdaFunctionFailedCause' name -> (error . showText) $ "Unknown StartLambdaFunctionFailedCause: " <> original name

-- | Represents the bounds of /known/ $StartLambdaFunctionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StartLambdaFunctionFailedCause where
    minBound = AssumeRoleFailed
    maxBound = AssumeRoleFailed

instance Hashable     StartLambdaFunctionFailedCause
instance NFData       StartLambdaFunctionFailedCause
instance ToByteString StartLambdaFunctionFailedCause
instance ToQuery      StartLambdaFunctionFailedCause
instance ToHeader     StartLambdaFunctionFailedCause

instance FromJSON StartLambdaFunctionFailedCause where
    parseJSON = parseJSONText "StartLambdaFunctionFailedCause"
