{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionTimeoutType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.LambdaFunctionTimeoutType (
  LambdaFunctionTimeoutType (
    ..
    , LFTTStartToClose
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LambdaFunctionTimeoutType = LambdaFunctionTimeoutType' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern LFTTStartToClose :: LambdaFunctionTimeoutType
pattern LFTTStartToClose = LambdaFunctionTimeoutType' "START_TO_CLOSE"

{-# COMPLETE
  LFTTStartToClose,
  LambdaFunctionTimeoutType' #-}

instance FromText LambdaFunctionTimeoutType where
    parser = (LambdaFunctionTimeoutType' . mk) <$> takeText

instance ToText LambdaFunctionTimeoutType where
    toText (LambdaFunctionTimeoutType' ci) = original ci

-- | Represents an enum of /known/ $LambdaFunctionTimeoutType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LambdaFunctionTimeoutType where
    toEnum i = case i of
        0 -> LFTTStartToClose
        _ -> (error . showText) $ "Unknown index for LambdaFunctionTimeoutType: " <> toText i
    fromEnum x = case x of
        LFTTStartToClose -> 0
        LambdaFunctionTimeoutType' name -> (error . showText) $ "Unknown LambdaFunctionTimeoutType: " <> original name

-- | Represents the bounds of /known/ $LambdaFunctionTimeoutType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LambdaFunctionTimeoutType where
    minBound = LFTTStartToClose
    maxBound = LFTTStartToClose

instance Hashable     LambdaFunctionTimeoutType
instance NFData       LambdaFunctionTimeoutType
instance ToByteString LambdaFunctionTimeoutType
instance ToQuery      LambdaFunctionTimeoutType
instance ToHeader     LambdaFunctionTimeoutType

instance FromJSON LambdaFunctionTimeoutType where
    parseJSON = parseJSONText "LambdaFunctionTimeoutType"
