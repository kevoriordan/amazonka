{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.StopAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.StopAction (
  StopAction (
    ..
    , SkipEvaluation
    , StartEvaluation
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StopAction = StopAction' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern SkipEvaluation :: StopAction
pattern SkipEvaluation = StopAction' "SKIP_EVALUATION"

pattern StartEvaluation :: StopAction
pattern StartEvaluation = StopAction' "START_EVALUATION"

{-# COMPLETE
  SkipEvaluation,
  StartEvaluation,
  StopAction' #-}

instance FromText StopAction where
    parser = (StopAction' . mk) <$> takeText

instance ToText StopAction where
    toText (StopAction' ci) = original ci

-- | Represents an enum of /known/ $StopAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StopAction where
    toEnum i = case i of
        0 -> SkipEvaluation
        1 -> StartEvaluation
        _ -> (error . showText) $ "Unknown index for StopAction: " <> toText i
    fromEnum x = case x of
        SkipEvaluation -> 0
        StartEvaluation -> 1
        StopAction' name -> (error . showText) $ "Unknown StopAction: " <> original name

-- | Represents the bounds of /known/ $StopAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StopAction where
    minBound = SkipEvaluation
    maxBound = StartEvaluation

instance Hashable     StopAction
instance NFData       StopAction
instance ToByteString StopAction
instance ToQuery      StopAction
instance ToHeader     StopAction

instance ToJSON StopAction where
    toJSON = toJSONText
