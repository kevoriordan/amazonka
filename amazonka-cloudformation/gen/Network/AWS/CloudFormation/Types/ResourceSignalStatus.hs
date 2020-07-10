{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceSignalStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ResourceSignalStatus (
  ResourceSignalStatus (
    ..
    , Failure
    , Success
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceSignalStatus = ResourceSignalStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Failure :: ResourceSignalStatus
pattern Failure = ResourceSignalStatus' "FAILURE"

pattern Success :: ResourceSignalStatus
pattern Success = ResourceSignalStatus' "SUCCESS"

{-# COMPLETE
  Failure,
  Success,
  ResourceSignalStatus' #-}

instance FromText ResourceSignalStatus where
    parser = (ResourceSignalStatus' . mk) <$> takeText

instance ToText ResourceSignalStatus where
    toText (ResourceSignalStatus' ci) = original ci

-- | Represents an enum of /known/ $ResourceSignalStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceSignalStatus where
    toEnum i = case i of
        0 -> Failure
        1 -> Success
        _ -> (error . showText) $ "Unknown index for ResourceSignalStatus: " <> toText i
    fromEnum x = case x of
        Failure -> 0
        Success -> 1
        ResourceSignalStatus' name -> (error . showText) $ "Unknown ResourceSignalStatus: " <> original name

-- | Represents the bounds of /known/ $ResourceSignalStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceSignalStatus where
    minBound = Failure
    maxBound = Success

instance Hashable     ResourceSignalStatus
instance NFData       ResourceSignalStatus
instance ToByteString ResourceSignalStatus
instance ToQuery      ResourceSignalStatus
instance ToHeader     ResourceSignalStatus
