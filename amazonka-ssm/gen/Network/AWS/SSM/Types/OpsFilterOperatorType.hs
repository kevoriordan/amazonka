{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsFilterOperatorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OpsFilterOperatorType (
  OpsFilterOperatorType (
    ..
    , BeginWith
    , Equal
    , Exists
    , GreaterThan
    , LessThan
    , NotEqual
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OpsFilterOperatorType = OpsFilterOperatorType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern BeginWith :: OpsFilterOperatorType
pattern BeginWith = OpsFilterOperatorType' "BeginWith"

pattern Equal :: OpsFilterOperatorType
pattern Equal = OpsFilterOperatorType' "Equal"

pattern Exists :: OpsFilterOperatorType
pattern Exists = OpsFilterOperatorType' "Exists"

pattern GreaterThan :: OpsFilterOperatorType
pattern GreaterThan = OpsFilterOperatorType' "GreaterThan"

pattern LessThan :: OpsFilterOperatorType
pattern LessThan = OpsFilterOperatorType' "LessThan"

pattern NotEqual :: OpsFilterOperatorType
pattern NotEqual = OpsFilterOperatorType' "NotEqual"

{-# COMPLETE
  BeginWith,
  Equal,
  Exists,
  GreaterThan,
  LessThan,
  NotEqual,
  OpsFilterOperatorType' #-}

instance FromText OpsFilterOperatorType where
    parser = (OpsFilterOperatorType' . mk) <$> takeText

instance ToText OpsFilterOperatorType where
    toText (OpsFilterOperatorType' ci) = original ci

-- | Represents an enum of /known/ $OpsFilterOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OpsFilterOperatorType where
    toEnum i = case i of
        0 -> BeginWith
        1 -> Equal
        2 -> Exists
        3 -> GreaterThan
        4 -> LessThan
        5 -> NotEqual
        _ -> (error . showText) $ "Unknown index for OpsFilterOperatorType: " <> toText i
    fromEnum x = case x of
        BeginWith -> 0
        Equal -> 1
        Exists -> 2
        GreaterThan -> 3
        LessThan -> 4
        NotEqual -> 5
        OpsFilterOperatorType' name -> (error . showText) $ "Unknown OpsFilterOperatorType: " <> original name

-- | Represents the bounds of /known/ $OpsFilterOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OpsFilterOperatorType where
    minBound = BeginWith
    maxBound = NotEqual

instance Hashable     OpsFilterOperatorType
instance NFData       OpsFilterOperatorType
instance ToByteString OpsFilterOperatorType
instance ToQuery      OpsFilterOperatorType
instance ToHeader     OpsFilterOperatorType

instance ToJSON OpsFilterOperatorType where
    toJSON = toJSONText
