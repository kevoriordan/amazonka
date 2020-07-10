{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RecordWrapper
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.RecordWrapper (
  RecordWrapper (
    ..
    , None
    , RecordIO
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RecordWrapper = RecordWrapper' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern None :: RecordWrapper
pattern None = RecordWrapper' "None"

pattern RecordIO :: RecordWrapper
pattern RecordIO = RecordWrapper' "RecordIO"

{-# COMPLETE
  None,
  RecordIO,
  RecordWrapper' #-}

instance FromText RecordWrapper where
    parser = (RecordWrapper' . mk) <$> takeText

instance ToText RecordWrapper where
    toText (RecordWrapper' ci) = original ci

-- | Represents an enum of /known/ $RecordWrapper.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RecordWrapper where
    toEnum i = case i of
        0 -> None
        1 -> RecordIO
        _ -> (error . showText) $ "Unknown index for RecordWrapper: " <> toText i
    fromEnum x = case x of
        None -> 0
        RecordIO -> 1
        RecordWrapper' name -> (error . showText) $ "Unknown RecordWrapper: " <> original name

-- | Represents the bounds of /known/ $RecordWrapper.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RecordWrapper where
    minBound = None
    maxBound = RecordIO

instance Hashable     RecordWrapper
instance NFData       RecordWrapper
instance ToByteString RecordWrapper
instance ToQuery      RecordWrapper
instance ToHeader     RecordWrapper

instance ToJSON RecordWrapper where
    toJSON = toJSONText

instance FromJSON RecordWrapper where
    parseJSON = parseJSONText "RecordWrapper"
