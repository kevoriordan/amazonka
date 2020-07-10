{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParametersFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ParametersFilterKey (
  ParametersFilterKey (
    ..
    , PFKKeyId
    , PFKName
    , PFKType
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ParametersFilterKey = ParametersFilterKey' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern PFKKeyId :: ParametersFilterKey
pattern PFKKeyId = ParametersFilterKey' "KeyId"

pattern PFKName :: ParametersFilterKey
pattern PFKName = ParametersFilterKey' "Name"

pattern PFKType :: ParametersFilterKey
pattern PFKType = ParametersFilterKey' "Type"

{-# COMPLETE
  PFKKeyId,
  PFKName,
  PFKType,
  ParametersFilterKey' #-}

instance FromText ParametersFilterKey where
    parser = (ParametersFilterKey' . mk) <$> takeText

instance ToText ParametersFilterKey where
    toText (ParametersFilterKey' ci) = original ci

-- | Represents an enum of /known/ $ParametersFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ParametersFilterKey where
    toEnum i = case i of
        0 -> PFKKeyId
        1 -> PFKName
        2 -> PFKType
        _ -> (error . showText) $ "Unknown index for ParametersFilterKey: " <> toText i
    fromEnum x = case x of
        PFKKeyId -> 0
        PFKName -> 1
        PFKType -> 2
        ParametersFilterKey' name -> (error . showText) $ "Unknown ParametersFilterKey: " <> original name

-- | Represents the bounds of /known/ $ParametersFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ParametersFilterKey where
    minBound = PFKKeyId
    maxBound = PFKType

instance Hashable     ParametersFilterKey
instance NFData       ParametersFilterKey
instance ToByteString ParametersFilterKey
instance ToQuery      ParametersFilterKey
instance ToHeader     ParametersFilterKey

instance ToJSON ParametersFilterKey where
    toJSON = toJSONText
