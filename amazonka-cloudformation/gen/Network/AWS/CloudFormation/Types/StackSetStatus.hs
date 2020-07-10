{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetStatus (
  StackSetStatus (
    ..
    , Active
    , Deleted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackSetStatus = StackSetStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Active :: StackSetStatus
pattern Active = StackSetStatus' "ACTIVE"

pattern Deleted :: StackSetStatus
pattern Deleted = StackSetStatus' "DELETED"

{-# COMPLETE
  Active,
  Deleted,
  StackSetStatus' #-}

instance FromText StackSetStatus where
    parser = (StackSetStatus' . mk) <$> takeText

instance ToText StackSetStatus where
    toText (StackSetStatus' ci) = original ci

-- | Represents an enum of /known/ $StackSetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackSetStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Deleted
        _ -> (error . showText) $ "Unknown index for StackSetStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Deleted -> 1
        StackSetStatus' name -> (error . showText) $ "Unknown StackSetStatus: " <> original name

-- | Represents the bounds of /known/ $StackSetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackSetStatus where
    minBound = Active
    maxBound = Deleted

instance Hashable     StackSetStatus
instance NFData       StackSetStatus
instance ToByteString StackSetStatus
instance ToQuery      StackSetStatus
instance ToHeader     StackSetStatus

instance FromXML StackSetStatus where
    parseXML = parseXMLText "StackSetStatus"
