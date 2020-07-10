{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.AccountJoinedMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.AccountJoinedMethod (
  AccountJoinedMethod (
    ..
    , Created
    , Invited
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AccountJoinedMethod = AccountJoinedMethod' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Created :: AccountJoinedMethod
pattern Created = AccountJoinedMethod' "CREATED"

pattern Invited :: AccountJoinedMethod
pattern Invited = AccountJoinedMethod' "INVITED"

{-# COMPLETE
  Created,
  Invited,
  AccountJoinedMethod' #-}

instance FromText AccountJoinedMethod where
    parser = (AccountJoinedMethod' . mk) <$> takeText

instance ToText AccountJoinedMethod where
    toText (AccountJoinedMethod' ci) = original ci

-- | Represents an enum of /known/ $AccountJoinedMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccountJoinedMethod where
    toEnum i = case i of
        0 -> Created
        1 -> Invited
        _ -> (error . showText) $ "Unknown index for AccountJoinedMethod: " <> toText i
    fromEnum x = case x of
        Created -> 0
        Invited -> 1
        AccountJoinedMethod' name -> (error . showText) $ "Unknown AccountJoinedMethod: " <> original name

-- | Represents the bounds of /known/ $AccountJoinedMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccountJoinedMethod where
    minBound = Created
    maxBound = Invited

instance Hashable     AccountJoinedMethod
instance NFData       AccountJoinedMethod
instance ToByteString AccountJoinedMethod
instance ToQuery      AccountJoinedMethod
instance ToHeader     AccountJoinedMethod

instance FromJSON AccountJoinedMethod where
    parseJSON = parseJSONText "AccountJoinedMethod"
