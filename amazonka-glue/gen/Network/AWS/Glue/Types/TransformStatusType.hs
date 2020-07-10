{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TransformStatusType (
  TransformStatusType (
    ..
    , Deleting
    , NotReady
    , Ready
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TransformStatusType = TransformStatusType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Deleting :: TransformStatusType
pattern Deleting = TransformStatusType' "DELETING"

pattern NotReady :: TransformStatusType
pattern NotReady = TransformStatusType' "NOT_READY"

pattern Ready :: TransformStatusType
pattern Ready = TransformStatusType' "READY"

{-# COMPLETE
  Deleting,
  NotReady,
  Ready,
  TransformStatusType' #-}

instance FromText TransformStatusType where
    parser = (TransformStatusType' . mk) <$> takeText

instance ToText TransformStatusType where
    toText (TransformStatusType' ci) = original ci

-- | Represents an enum of /known/ $TransformStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransformStatusType where
    toEnum i = case i of
        0 -> Deleting
        1 -> NotReady
        2 -> Ready
        _ -> (error . showText) $ "Unknown index for TransformStatusType: " <> toText i
    fromEnum x = case x of
        Deleting -> 0
        NotReady -> 1
        Ready -> 2
        TransformStatusType' name -> (error . showText) $ "Unknown TransformStatusType: " <> original name

-- | Represents the bounds of /known/ $TransformStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransformStatusType where
    minBound = Deleting
    maxBound = Ready

instance Hashable     TransformStatusType
instance NFData       TransformStatusType
instance ToByteString TransformStatusType
instance ToQuery      TransformStatusType
instance ToHeader     TransformStatusType

instance ToJSON TransformStatusType where
    toJSON = toJSONText

instance FromJSON TransformStatusType where
    parseJSON = parseJSONText "TransformStatusType"
