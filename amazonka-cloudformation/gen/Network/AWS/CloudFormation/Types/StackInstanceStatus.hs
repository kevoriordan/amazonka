{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackInstanceStatus (
  StackInstanceStatus (
    ..
    , Current
    , Inoperable
    , Outdated
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackInstanceStatus = StackInstanceStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Current :: StackInstanceStatus
pattern Current = StackInstanceStatus' "CURRENT"

pattern Inoperable :: StackInstanceStatus
pattern Inoperable = StackInstanceStatus' "INOPERABLE"

pattern Outdated :: StackInstanceStatus
pattern Outdated = StackInstanceStatus' "OUTDATED"

{-# COMPLETE
  Current,
  Inoperable,
  Outdated,
  StackInstanceStatus' #-}

instance FromText StackInstanceStatus where
    parser = (StackInstanceStatus' . mk) <$> takeText

instance ToText StackInstanceStatus where
    toText (StackInstanceStatus' ci) = original ci

-- | Represents an enum of /known/ $StackInstanceStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackInstanceStatus where
    toEnum i = case i of
        0 -> Current
        1 -> Inoperable
        2 -> Outdated
        _ -> (error . showText) $ "Unknown index for StackInstanceStatus: " <> toText i
    fromEnum x = case x of
        Current -> 0
        Inoperable -> 1
        Outdated -> 2
        StackInstanceStatus' name -> (error . showText) $ "Unknown StackInstanceStatus: " <> original name

-- | Represents the bounds of /known/ $StackInstanceStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackInstanceStatus where
    minBound = Current
    maxBound = Outdated

instance Hashable     StackInstanceStatus
instance NFData       StackInstanceStatus
instance ToByteString StackInstanceStatus
instance ToQuery      StackInstanceStatus
instance ToHeader     StackInstanceStatus

instance FromXML StackInstanceStatus where
    parseXML = parseXMLText "StackInstanceStatus"
