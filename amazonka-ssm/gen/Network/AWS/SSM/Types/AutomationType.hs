{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AutomationType (
  AutomationType (
    ..
    , CrossAccount
    , Local
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutomationType = AutomationType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern CrossAccount :: AutomationType
pattern CrossAccount = AutomationType' "CrossAccount"

pattern Local :: AutomationType
pattern Local = AutomationType' "Local"

{-# COMPLETE
  CrossAccount,
  Local,
  AutomationType' #-}

instance FromText AutomationType where
    parser = (AutomationType' . mk) <$> takeText

instance ToText AutomationType where
    toText (AutomationType' ci) = original ci

-- | Represents an enum of /known/ $AutomationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutomationType where
    toEnum i = case i of
        0 -> CrossAccount
        1 -> Local
        _ -> (error . showText) $ "Unknown index for AutomationType: " <> toText i
    fromEnum x = case x of
        CrossAccount -> 0
        Local -> 1
        AutomationType' name -> (error . showText) $ "Unknown AutomationType: " <> original name

-- | Represents the bounds of /known/ $AutomationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutomationType where
    minBound = CrossAccount
    maxBound = Local

instance Hashable     AutomationType
instance NFData       AutomationType
instance ToByteString AutomationType
instance ToQuery      AutomationType
instance ToHeader     AutomationType

instance FromJSON AutomationType where
    parseJSON = parseJSONText "AutomationType"
