{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType (
  CompromisedCredentialsEventActionType (
    ..
    , CCEATBlock
    , CCEATNoAction
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CompromisedCredentialsEventActionType = CompromisedCredentialsEventActionType' (CI
                                                                                       Text)
                                               deriving (Eq, Ord, Read, Show,
                                                         Data, Typeable,
                                                         Generic)

pattern CCEATBlock :: CompromisedCredentialsEventActionType
pattern CCEATBlock = CompromisedCredentialsEventActionType' "BLOCK"

pattern CCEATNoAction :: CompromisedCredentialsEventActionType
pattern CCEATNoAction = CompromisedCredentialsEventActionType' "NO_ACTION"

{-# COMPLETE
  CCEATBlock,
  CCEATNoAction,
  CompromisedCredentialsEventActionType' #-}

instance FromText CompromisedCredentialsEventActionType where
    parser = (CompromisedCredentialsEventActionType' . mk) <$> takeText

instance ToText CompromisedCredentialsEventActionType where
    toText (CompromisedCredentialsEventActionType' ci) = original ci

-- | Represents an enum of /known/ $CompromisedCredentialsEventActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CompromisedCredentialsEventActionType where
    toEnum i = case i of
        0 -> CCEATBlock
        1 -> CCEATNoAction
        _ -> (error . showText) $ "Unknown index for CompromisedCredentialsEventActionType: " <> toText i
    fromEnum x = case x of
        CCEATBlock -> 0
        CCEATNoAction -> 1
        CompromisedCredentialsEventActionType' name -> (error . showText) $ "Unknown CompromisedCredentialsEventActionType: " <> original name

-- | Represents the bounds of /known/ $CompromisedCredentialsEventActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CompromisedCredentialsEventActionType where
    minBound = CCEATBlock
    maxBound = CCEATNoAction

instance Hashable     CompromisedCredentialsEventActionType
instance NFData       CompromisedCredentialsEventActionType
instance ToByteString CompromisedCredentialsEventActionType
instance ToQuery      CompromisedCredentialsEventActionType
instance ToHeader     CompromisedCredentialsEventActionType

instance ToJSON CompromisedCredentialsEventActionType where
    toJSON = toJSONText

instance FromJSON CompromisedCredentialsEventActionType where
    parseJSON = parseJSONText "CompromisedCredentialsEventActionType"
