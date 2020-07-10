{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes (
  PreventUserExistenceErrorTypes (
    ..
    , PUEETEnabled
    , PUEETLegacy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PreventUserExistenceErrorTypes = PreventUserExistenceErrorTypes' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern PUEETEnabled :: PreventUserExistenceErrorTypes
pattern PUEETEnabled = PreventUserExistenceErrorTypes' "ENABLED"

pattern PUEETLegacy :: PreventUserExistenceErrorTypes
pattern PUEETLegacy = PreventUserExistenceErrorTypes' "LEGACY"

{-# COMPLETE
  PUEETEnabled,
  PUEETLegacy,
  PreventUserExistenceErrorTypes' #-}

instance FromText PreventUserExistenceErrorTypes where
    parser = (PreventUserExistenceErrorTypes' . mk) <$> takeText

instance ToText PreventUserExistenceErrorTypes where
    toText (PreventUserExistenceErrorTypes' ci) = original ci

-- | Represents an enum of /known/ $PreventUserExistenceErrorTypes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PreventUserExistenceErrorTypes where
    toEnum i = case i of
        0 -> PUEETEnabled
        1 -> PUEETLegacy
        _ -> (error . showText) $ "Unknown index for PreventUserExistenceErrorTypes: " <> toText i
    fromEnum x = case x of
        PUEETEnabled -> 0
        PUEETLegacy -> 1
        PreventUserExistenceErrorTypes' name -> (error . showText) $ "Unknown PreventUserExistenceErrorTypes: " <> original name

-- | Represents the bounds of /known/ $PreventUserExistenceErrorTypes.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PreventUserExistenceErrorTypes where
    minBound = PUEETEnabled
    maxBound = PUEETLegacy

instance Hashable     PreventUserExistenceErrorTypes
instance NFData       PreventUserExistenceErrorTypes
instance ToByteString PreventUserExistenceErrorTypes
instance ToQuery      PreventUserExistenceErrorTypes
instance ToHeader     PreventUserExistenceErrorTypes

instance ToJSON PreventUserExistenceErrorTypes where
    toJSON = toJSONText

instance FromJSON PreventUserExistenceErrorTypes where
    parseJSON = parseJSONText "PreventUserExistenceErrorTypes"
