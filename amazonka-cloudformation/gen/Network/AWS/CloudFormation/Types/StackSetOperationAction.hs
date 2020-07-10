{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetOperationAction (
  StackSetOperationAction (
    ..
    , Create
    , Delete
    , DetectDrift
    , Update
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackSetOperationAction = StackSetOperationAction' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Create :: StackSetOperationAction
pattern Create = StackSetOperationAction' "CREATE"

pattern Delete :: StackSetOperationAction
pattern Delete = StackSetOperationAction' "DELETE"

pattern DetectDrift :: StackSetOperationAction
pattern DetectDrift = StackSetOperationAction' "DETECT_DRIFT"

pattern Update :: StackSetOperationAction
pattern Update = StackSetOperationAction' "UPDATE"

{-# COMPLETE
  Create,
  Delete,
  DetectDrift,
  Update,
  StackSetOperationAction' #-}

instance FromText StackSetOperationAction where
    parser = (StackSetOperationAction' . mk) <$> takeText

instance ToText StackSetOperationAction where
    toText (StackSetOperationAction' ci) = original ci

-- | Represents an enum of /known/ $StackSetOperationAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackSetOperationAction where
    toEnum i = case i of
        0 -> Create
        1 -> Delete
        2 -> DetectDrift
        3 -> Update
        _ -> (error . showText) $ "Unknown index for StackSetOperationAction: " <> toText i
    fromEnum x = case x of
        Create -> 0
        Delete -> 1
        DetectDrift -> 2
        Update -> 3
        StackSetOperationAction' name -> (error . showText) $ "Unknown StackSetOperationAction: " <> original name

-- | Represents the bounds of /known/ $StackSetOperationAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackSetOperationAction where
    minBound = Create
    maxBound = Update

instance Hashable     StackSetOperationAction
instance NFData       StackSetOperationAction
instance ToByteString StackSetOperationAction
instance ToQuery      StackSetOperationAction
instance ToHeader     StackSetOperationAction

instance FromXML StackSetOperationAction where
    parseXML = parseXMLText "StackSetOperationAction"
