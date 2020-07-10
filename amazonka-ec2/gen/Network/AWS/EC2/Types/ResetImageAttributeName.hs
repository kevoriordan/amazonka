{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResetImageAttributeName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ResetImageAttributeName (
  ResetImageAttributeName (
    ..
    , RIANLaunchPermission
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ResetImageAttributeName = ResetImageAttributeName' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern RIANLaunchPermission :: ResetImageAttributeName
pattern RIANLaunchPermission = ResetImageAttributeName' "launchPermission"

{-# COMPLETE
  RIANLaunchPermission,
  ResetImageAttributeName' #-}

instance FromText ResetImageAttributeName where
    parser = (ResetImageAttributeName' . mk) <$> takeText

instance ToText ResetImageAttributeName where
    toText (ResetImageAttributeName' ci) = original ci

-- | Represents an enum of /known/ $ResetImageAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResetImageAttributeName where
    toEnum i = case i of
        0 -> RIANLaunchPermission
        _ -> (error . showText) $ "Unknown index for ResetImageAttributeName: " <> toText i
    fromEnum x = case x of
        RIANLaunchPermission -> 0
        ResetImageAttributeName' name -> (error . showText) $ "Unknown ResetImageAttributeName: " <> original name

-- | Represents the bounds of /known/ $ResetImageAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResetImageAttributeName where
    minBound = RIANLaunchPermission
    maxBound = RIANLaunchPermission

instance Hashable     ResetImageAttributeName
instance NFData       ResetImageAttributeName
instance ToByteString ResetImageAttributeName
instance ToQuery      ResetImageAttributeName
instance ToHeader     ResetImageAttributeName
