{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceQueryOperatorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ComplianceQueryOperatorType (
  ComplianceQueryOperatorType (
    ..
    , CQOTBeginWith
    , CQOTEqual
    , CQOTGreaterThan
    , CQOTLessThan
    , CQOTNotEqual
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComplianceQueryOperatorType = ComplianceQueryOperatorType' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern CQOTBeginWith :: ComplianceQueryOperatorType
pattern CQOTBeginWith = ComplianceQueryOperatorType' "BEGIN_WITH"

pattern CQOTEqual :: ComplianceQueryOperatorType
pattern CQOTEqual = ComplianceQueryOperatorType' "EQUAL"

pattern CQOTGreaterThan :: ComplianceQueryOperatorType
pattern CQOTGreaterThan = ComplianceQueryOperatorType' "GREATER_THAN"

pattern CQOTLessThan :: ComplianceQueryOperatorType
pattern CQOTLessThan = ComplianceQueryOperatorType' "LESS_THAN"

pattern CQOTNotEqual :: ComplianceQueryOperatorType
pattern CQOTNotEqual = ComplianceQueryOperatorType' "NOT_EQUAL"

{-# COMPLETE
  CQOTBeginWith,
  CQOTEqual,
  CQOTGreaterThan,
  CQOTLessThan,
  CQOTNotEqual,
  ComplianceQueryOperatorType' #-}

instance FromText ComplianceQueryOperatorType where
    parser = (ComplianceQueryOperatorType' . mk) <$> takeText

instance ToText ComplianceQueryOperatorType where
    toText (ComplianceQueryOperatorType' ci) = original ci

-- | Represents an enum of /known/ $ComplianceQueryOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ComplianceQueryOperatorType where
    toEnum i = case i of
        0 -> CQOTBeginWith
        1 -> CQOTEqual
        2 -> CQOTGreaterThan
        3 -> CQOTLessThan
        4 -> CQOTNotEqual
        _ -> (error . showText) $ "Unknown index for ComplianceQueryOperatorType: " <> toText i
    fromEnum x = case x of
        CQOTBeginWith -> 0
        CQOTEqual -> 1
        CQOTGreaterThan -> 2
        CQOTLessThan -> 3
        CQOTNotEqual -> 4
        ComplianceQueryOperatorType' name -> (error . showText) $ "Unknown ComplianceQueryOperatorType: " <> original name

-- | Represents the bounds of /known/ $ComplianceQueryOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComplianceQueryOperatorType where
    minBound = CQOTBeginWith
    maxBound = CQOTNotEqual

instance Hashable     ComplianceQueryOperatorType
instance NFData       ComplianceQueryOperatorType
instance ToByteString ComplianceQueryOperatorType
instance ToQuery      ComplianceQueryOperatorType
instance ToHeader     ComplianceQueryOperatorType

instance ToJSON ComplianceQueryOperatorType where
    toJSON = toJSONText
