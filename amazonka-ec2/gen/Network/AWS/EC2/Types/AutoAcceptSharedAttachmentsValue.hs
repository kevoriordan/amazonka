{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AutoAcceptSharedAttachmentsValue (
  AutoAcceptSharedAttachmentsValue (
    ..
    , Disable
    , Enable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AutoAcceptSharedAttachmentsValue = AutoAcceptSharedAttachmentsValue' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern Disable :: AutoAcceptSharedAttachmentsValue
pattern Disable = AutoAcceptSharedAttachmentsValue' "disable"

pattern Enable :: AutoAcceptSharedAttachmentsValue
pattern Enable = AutoAcceptSharedAttachmentsValue' "enable"

{-# COMPLETE
  Disable,
  Enable,
  AutoAcceptSharedAttachmentsValue' #-}

instance FromText AutoAcceptSharedAttachmentsValue where
    parser = (AutoAcceptSharedAttachmentsValue' . mk) <$> takeText

instance ToText AutoAcceptSharedAttachmentsValue where
    toText (AutoAcceptSharedAttachmentsValue' ci) = original ci

-- | Represents an enum of /known/ $AutoAcceptSharedAttachmentsValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoAcceptSharedAttachmentsValue where
    toEnum i = case i of
        0 -> Disable
        1 -> Enable
        _ -> (error . showText) $ "Unknown index for AutoAcceptSharedAttachmentsValue: " <> toText i
    fromEnum x = case x of
        Disable -> 0
        Enable -> 1
        AutoAcceptSharedAttachmentsValue' name -> (error . showText) $ "Unknown AutoAcceptSharedAttachmentsValue: " <> original name

-- | Represents the bounds of /known/ $AutoAcceptSharedAttachmentsValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoAcceptSharedAttachmentsValue where
    minBound = Disable
    maxBound = Enable

instance Hashable     AutoAcceptSharedAttachmentsValue
instance NFData       AutoAcceptSharedAttachmentsValue
instance ToByteString AutoAcceptSharedAttachmentsValue
instance ToQuery      AutoAcceptSharedAttachmentsValue
instance ToHeader     AutoAcceptSharedAttachmentsValue

instance FromXML AutoAcceptSharedAttachmentsValue where
    parseXML = parseXMLText "AutoAcceptSharedAttachmentsValue"
