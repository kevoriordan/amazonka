{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.CreateAccountState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.CreateAccountState (
  CreateAccountState (
    ..
    , Failed
    , InProgress
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CreateAccountState = CreateAccountState' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Failed :: CreateAccountState
pattern Failed = CreateAccountState' "FAILED"

pattern InProgress :: CreateAccountState
pattern InProgress = CreateAccountState' "IN_PROGRESS"

pattern Succeeded :: CreateAccountState
pattern Succeeded = CreateAccountState' "SUCCEEDED"

{-# COMPLETE
  Failed,
  InProgress,
  Succeeded,
  CreateAccountState' #-}

instance FromText CreateAccountState where
    parser = (CreateAccountState' . mk) <$> takeText

instance ToText CreateAccountState where
    toText (CreateAccountState' ci) = original ci

-- | Represents an enum of /known/ $CreateAccountState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CreateAccountState where
    toEnum i = case i of
        0 -> Failed
        1 -> InProgress
        2 -> Succeeded
        _ -> (error . showText) $ "Unknown index for CreateAccountState: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        InProgress -> 1
        Succeeded -> 2
        CreateAccountState' name -> (error . showText) $ "Unknown CreateAccountState: " <> original name

-- | Represents the bounds of /known/ $CreateAccountState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CreateAccountState where
    minBound = Failed
    maxBound = Succeeded

instance Hashable     CreateAccountState
instance NFData       CreateAccountState
instance ToByteString CreateAccountState
instance ToQuery      CreateAccountState
instance ToHeader     CreateAccountState

instance ToJSON CreateAccountState where
    toJSON = toJSONText

instance FromJSON CreateAccountState where
    parseJSON = parseJSONText "CreateAccountState"
