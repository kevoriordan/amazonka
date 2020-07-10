{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.SanitizationWarningReason
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.SanitizationWarningReason (
  SanitizationWarningReason (
    ..
    , DisallowedAttributeRemoved
    , DisallowedElementRemoved
    , InvalidAttributeValueRemoved
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The reason for which the XML elements or attributes were sanitized.
data SanitizationWarningReason = SanitizationWarningReason' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern DisallowedAttributeRemoved :: SanitizationWarningReason
pattern DisallowedAttributeRemoved = SanitizationWarningReason' "DISALLOWED_ATTRIBUTE_REMOVED"

pattern DisallowedElementRemoved :: SanitizationWarningReason
pattern DisallowedElementRemoved = SanitizationWarningReason' "DISALLOWED_ELEMENT_REMOVED"

pattern InvalidAttributeValueRemoved :: SanitizationWarningReason
pattern InvalidAttributeValueRemoved = SanitizationWarningReason' "INVALID_ATTRIBUTE_VALUE_REMOVED"

{-# COMPLETE
  DisallowedAttributeRemoved,
  DisallowedElementRemoved,
  InvalidAttributeValueRemoved,
  SanitizationWarningReason' #-}

instance FromText SanitizationWarningReason where
    parser = (SanitizationWarningReason' . mk) <$> takeText

instance ToText SanitizationWarningReason where
    toText (SanitizationWarningReason' ci) = original ci

-- | Represents an enum of /known/ $SanitizationWarningReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SanitizationWarningReason where
    toEnum i = case i of
        0 -> DisallowedAttributeRemoved
        1 -> DisallowedElementRemoved
        2 -> InvalidAttributeValueRemoved
        _ -> (error . showText) $ "Unknown index for SanitizationWarningReason: " <> toText i
    fromEnum x = case x of
        DisallowedAttributeRemoved -> 0
        DisallowedElementRemoved -> 1
        InvalidAttributeValueRemoved -> 2
        SanitizationWarningReason' name -> (error . showText) $ "Unknown SanitizationWarningReason: " <> original name

-- | Represents the bounds of /known/ $SanitizationWarningReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SanitizationWarningReason where
    minBound = DisallowedAttributeRemoved
    maxBound = InvalidAttributeValueRemoved

instance Hashable     SanitizationWarningReason
instance NFData       SanitizationWarningReason
instance ToByteString SanitizationWarningReason
instance ToQuery      SanitizationWarningReason
instance ToHeader     SanitizationWarningReason

instance FromJSON SanitizationWarningReason where
    parseJSON = parseJSONText "SanitizationWarningReason"
