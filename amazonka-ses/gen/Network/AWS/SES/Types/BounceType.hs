{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BounceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.BounceType (
  BounceType (
    ..
    , ContentRejected
    , DoesNotExist
    , ExceededQuota
    , MessageTooLarge
    , TemporaryFailure
    , Undefined
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BounceType = BounceType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ContentRejected :: BounceType
pattern ContentRejected = BounceType' "ContentRejected"

pattern DoesNotExist :: BounceType
pattern DoesNotExist = BounceType' "DoesNotExist"

pattern ExceededQuota :: BounceType
pattern ExceededQuota = BounceType' "ExceededQuota"

pattern MessageTooLarge :: BounceType
pattern MessageTooLarge = BounceType' "MessageTooLarge"

pattern TemporaryFailure :: BounceType
pattern TemporaryFailure = BounceType' "TemporaryFailure"

pattern Undefined :: BounceType
pattern Undefined = BounceType' "Undefined"

{-# COMPLETE
  ContentRejected,
  DoesNotExist,
  ExceededQuota,
  MessageTooLarge,
  TemporaryFailure,
  Undefined,
  BounceType' #-}

instance FromText BounceType where
    parser = (BounceType' . mk) <$> takeText

instance ToText BounceType where
    toText (BounceType' ci) = original ci

-- | Represents an enum of /known/ $BounceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BounceType where
    toEnum i = case i of
        0 -> ContentRejected
        1 -> DoesNotExist
        2 -> ExceededQuota
        3 -> MessageTooLarge
        4 -> TemporaryFailure
        5 -> Undefined
        _ -> (error . showText) $ "Unknown index for BounceType: " <> toText i
    fromEnum x = case x of
        ContentRejected -> 0
        DoesNotExist -> 1
        ExceededQuota -> 2
        MessageTooLarge -> 3
        TemporaryFailure -> 4
        Undefined -> 5
        BounceType' name -> (error . showText) $ "Unknown BounceType: " <> original name

-- | Represents the bounds of /known/ $BounceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BounceType where
    minBound = ContentRejected
    maxBound = Undefined

instance Hashable     BounceType
instance NFData       BounceType
instance ToByteString BounceType
instance ToQuery      BounceType
instance ToHeader     BounceType
