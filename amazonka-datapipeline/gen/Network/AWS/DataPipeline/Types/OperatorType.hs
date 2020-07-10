{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.OperatorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.OperatorType (
  OperatorType (
    ..
    , OperatorBetween
    , OperatorEQ'
    , OperatorGE
    , OperatorLE
    , OperatorRefEQ
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OperatorType = OperatorType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern OperatorBetween :: OperatorType
pattern OperatorBetween = OperatorType' "BETWEEN"

pattern OperatorEQ' :: OperatorType
pattern OperatorEQ' = OperatorType' "EQ"

pattern OperatorGE :: OperatorType
pattern OperatorGE = OperatorType' "GE"

pattern OperatorLE :: OperatorType
pattern OperatorLE = OperatorType' "LE"

pattern OperatorRefEQ :: OperatorType
pattern OperatorRefEQ = OperatorType' "REF_EQ"

{-# COMPLETE
  OperatorBetween,
  OperatorEQ',
  OperatorGE,
  OperatorLE,
  OperatorRefEQ,
  OperatorType' #-}

instance FromText OperatorType where
    parser = (OperatorType' . mk) <$> takeText

instance ToText OperatorType where
    toText (OperatorType' ci) = original ci

-- | Represents an enum of /known/ $OperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OperatorType where
    toEnum i = case i of
        0 -> OperatorBetween
        1 -> OperatorEQ'
        2 -> OperatorGE
        3 -> OperatorLE
        4 -> OperatorRefEQ
        _ -> (error . showText) $ "Unknown index for OperatorType: " <> toText i
    fromEnum x = case x of
        OperatorBetween -> 0
        OperatorEQ' -> 1
        OperatorGE -> 2
        OperatorLE -> 3
        OperatorRefEQ -> 4
        OperatorType' name -> (error . showText) $ "Unknown OperatorType: " <> original name

-- | Represents the bounds of /known/ $OperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OperatorType where
    minBound = OperatorBetween
    maxBound = OperatorRefEQ

instance Hashable     OperatorType
instance NFData       OperatorType
instance ToByteString OperatorType
instance ToQuery      OperatorType
instance ToHeader     OperatorType

instance ToJSON OperatorType where
    toJSON = toJSONText
