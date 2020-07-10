{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType (
  AuthTokenUpdateStrategyType (
    ..
    , Rotate
    , Set
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthTokenUpdateStrategyType = AuthTokenUpdateStrategyType' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern Rotate :: AuthTokenUpdateStrategyType
pattern Rotate = AuthTokenUpdateStrategyType' "ROTATE"

pattern Set :: AuthTokenUpdateStrategyType
pattern Set = AuthTokenUpdateStrategyType' "SET"

{-# COMPLETE
  Rotate,
  Set,
  AuthTokenUpdateStrategyType' #-}

instance FromText AuthTokenUpdateStrategyType where
    parser = (AuthTokenUpdateStrategyType' . mk) <$> takeText

instance ToText AuthTokenUpdateStrategyType where
    toText (AuthTokenUpdateStrategyType' ci) = original ci

-- | Represents an enum of /known/ $AuthTokenUpdateStrategyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthTokenUpdateStrategyType where
    toEnum i = case i of
        0 -> Rotate
        1 -> Set
        _ -> (error . showText) $ "Unknown index for AuthTokenUpdateStrategyType: " <> toText i
    fromEnum x = case x of
        Rotate -> 0
        Set -> 1
        AuthTokenUpdateStrategyType' name -> (error . showText) $ "Unknown AuthTokenUpdateStrategyType: " <> original name

-- | Represents the bounds of /known/ $AuthTokenUpdateStrategyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthTokenUpdateStrategyType where
    minBound = Rotate
    maxBound = Set

instance Hashable     AuthTokenUpdateStrategyType
instance NFData       AuthTokenUpdateStrategyType
instance ToByteString AuthTokenUpdateStrategyType
instance ToQuery      AuthTokenUpdateStrategyType
instance ToHeader     AuthTokenUpdateStrategyType
