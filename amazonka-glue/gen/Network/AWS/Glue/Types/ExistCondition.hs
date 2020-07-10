{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ExistCondition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ExistCondition (
  ExistCondition (
    ..
    , MustExist
    , None
    , NotExist
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExistCondition = ExistCondition' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern MustExist :: ExistCondition
pattern MustExist = ExistCondition' "MUST_EXIST"

pattern None :: ExistCondition
pattern None = ExistCondition' "NONE"

pattern NotExist :: ExistCondition
pattern NotExist = ExistCondition' "NOT_EXIST"

{-# COMPLETE
  MustExist,
  None,
  NotExist,
  ExistCondition' #-}

instance FromText ExistCondition where
    parser = (ExistCondition' . mk) <$> takeText

instance ToText ExistCondition where
    toText (ExistCondition' ci) = original ci

-- | Represents an enum of /known/ $ExistCondition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExistCondition where
    toEnum i = case i of
        0 -> MustExist
        1 -> None
        2 -> NotExist
        _ -> (error . showText) $ "Unknown index for ExistCondition: " <> toText i
    fromEnum x = case x of
        MustExist -> 0
        None -> 1
        NotExist -> 2
        ExistCondition' name -> (error . showText) $ "Unknown ExistCondition: " <> original name

-- | Represents the bounds of /known/ $ExistCondition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExistCondition where
    minBound = MustExist
    maxBound = NotExist

instance Hashable     ExistCondition
instance NFData       ExistCondition
instance ToByteString ExistCondition
instance ToQuery      ExistCondition
instance ToHeader     ExistCondition

instance ToJSON ExistCondition where
    toJSON = toJSONText
