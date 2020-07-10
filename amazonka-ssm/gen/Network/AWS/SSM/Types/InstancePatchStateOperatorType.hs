{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstancePatchStateOperatorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InstancePatchStateOperatorType (
  InstancePatchStateOperatorType (
    ..
    , IPSOTEqual
    , IPSOTGreaterThan
    , IPSOTLessThan
    , IPSOTNotEqual
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstancePatchStateOperatorType = InstancePatchStateOperatorType' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern IPSOTEqual :: InstancePatchStateOperatorType
pattern IPSOTEqual = InstancePatchStateOperatorType' "Equal"

pattern IPSOTGreaterThan :: InstancePatchStateOperatorType
pattern IPSOTGreaterThan = InstancePatchStateOperatorType' "GreaterThan"

pattern IPSOTLessThan :: InstancePatchStateOperatorType
pattern IPSOTLessThan = InstancePatchStateOperatorType' "LessThan"

pattern IPSOTNotEqual :: InstancePatchStateOperatorType
pattern IPSOTNotEqual = InstancePatchStateOperatorType' "NotEqual"

{-# COMPLETE
  IPSOTEqual,
  IPSOTGreaterThan,
  IPSOTLessThan,
  IPSOTNotEqual,
  InstancePatchStateOperatorType' #-}

instance FromText InstancePatchStateOperatorType where
    parser = (InstancePatchStateOperatorType' . mk) <$> takeText

instance ToText InstancePatchStateOperatorType where
    toText (InstancePatchStateOperatorType' ci) = original ci

-- | Represents an enum of /known/ $InstancePatchStateOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstancePatchStateOperatorType where
    toEnum i = case i of
        0 -> IPSOTEqual
        1 -> IPSOTGreaterThan
        2 -> IPSOTLessThan
        3 -> IPSOTNotEqual
        _ -> (error . showText) $ "Unknown index for InstancePatchStateOperatorType: " <> toText i
    fromEnum x = case x of
        IPSOTEqual -> 0
        IPSOTGreaterThan -> 1
        IPSOTLessThan -> 2
        IPSOTNotEqual -> 3
        InstancePatchStateOperatorType' name -> (error . showText) $ "Unknown InstancePatchStateOperatorType: " <> original name

-- | Represents the bounds of /known/ $InstancePatchStateOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstancePatchStateOperatorType where
    minBound = IPSOTEqual
    maxBound = IPSOTNotEqual

instance Hashable     InstancePatchStateOperatorType
instance NFData       InstancePatchStateOperatorType
instance ToByteString InstancePatchStateOperatorType
instance ToQuery      InstancePatchStateOperatorType
instance ToHeader     InstancePatchStateOperatorType

instance ToJSON InstancePatchStateOperatorType where
    toJSON = toJSONText
