{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationFilterOperatorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationFilterOperatorType (
  AssociationFilterOperatorType (
    ..
    , AFOTEqual
    , AFOTGreaterThan
    , AFOTLessThan
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssociationFilterOperatorType = AssociationFilterOperatorType' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern AFOTEqual :: AssociationFilterOperatorType
pattern AFOTEqual = AssociationFilterOperatorType' "EQUAL"

pattern AFOTGreaterThan :: AssociationFilterOperatorType
pattern AFOTGreaterThan = AssociationFilterOperatorType' "GREATER_THAN"

pattern AFOTLessThan :: AssociationFilterOperatorType
pattern AFOTLessThan = AssociationFilterOperatorType' "LESS_THAN"

{-# COMPLETE
  AFOTEqual,
  AFOTGreaterThan,
  AFOTLessThan,
  AssociationFilterOperatorType' #-}

instance FromText AssociationFilterOperatorType where
    parser = (AssociationFilterOperatorType' . mk) <$> takeText

instance ToText AssociationFilterOperatorType where
    toText (AssociationFilterOperatorType' ci) = original ci

-- | Represents an enum of /known/ $AssociationFilterOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssociationFilterOperatorType where
    toEnum i = case i of
        0 -> AFOTEqual
        1 -> AFOTGreaterThan
        2 -> AFOTLessThan
        _ -> (error . showText) $ "Unknown index for AssociationFilterOperatorType: " <> toText i
    fromEnum x = case x of
        AFOTEqual -> 0
        AFOTGreaterThan -> 1
        AFOTLessThan -> 2
        AssociationFilterOperatorType' name -> (error . showText) $ "Unknown AssociationFilterOperatorType: " <> original name

-- | Represents the bounds of /known/ $AssociationFilterOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssociationFilterOperatorType where
    minBound = AFOTEqual
    maxBound = AFOTLessThan

instance Hashable     AssociationFilterOperatorType
instance NFData       AssociationFilterOperatorType
instance ToByteString AssociationFilterOperatorType
instance ToQuery      AssociationFilterOperatorType
instance ToHeader     AssociationFilterOperatorType

instance ToJSON AssociationFilterOperatorType where
    toJSON = toJSONText
