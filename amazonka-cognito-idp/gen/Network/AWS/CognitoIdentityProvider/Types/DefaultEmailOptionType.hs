{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType (
  DefaultEmailOptionType (
    ..
    , ConfirmWithCode
    , ConfirmWithLink
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DefaultEmailOptionType = DefaultEmailOptionType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern ConfirmWithCode :: DefaultEmailOptionType
pattern ConfirmWithCode = DefaultEmailOptionType' "CONFIRM_WITH_CODE"

pattern ConfirmWithLink :: DefaultEmailOptionType
pattern ConfirmWithLink = DefaultEmailOptionType' "CONFIRM_WITH_LINK"

{-# COMPLETE
  ConfirmWithCode,
  ConfirmWithLink,
  DefaultEmailOptionType' #-}

instance FromText DefaultEmailOptionType where
    parser = (DefaultEmailOptionType' . mk) <$> takeText

instance ToText DefaultEmailOptionType where
    toText (DefaultEmailOptionType' ci) = original ci

-- | Represents an enum of /known/ $DefaultEmailOptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DefaultEmailOptionType where
    toEnum i = case i of
        0 -> ConfirmWithCode
        1 -> ConfirmWithLink
        _ -> (error . showText) $ "Unknown index for DefaultEmailOptionType: " <> toText i
    fromEnum x = case x of
        ConfirmWithCode -> 0
        ConfirmWithLink -> 1
        DefaultEmailOptionType' name -> (error . showText) $ "Unknown DefaultEmailOptionType: " <> original name

-- | Represents the bounds of /known/ $DefaultEmailOptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DefaultEmailOptionType where
    minBound = ConfirmWithCode
    maxBound = ConfirmWithLink

instance Hashable     DefaultEmailOptionType
instance NFData       DefaultEmailOptionType
instance ToByteString DefaultEmailOptionType
instance ToQuery      DefaultEmailOptionType
instance ToHeader     DefaultEmailOptionType

instance ToJSON DefaultEmailOptionType where
    toJSON = toJSONText

instance FromJSON DefaultEmailOptionType where
    parseJSON = parseJSONText "DefaultEmailOptionType"
