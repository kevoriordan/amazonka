{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.DimensionValueSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.DimensionValueSource (
  DimensionValueSource (
    ..
    , EmailHeader
    , LinkTag
    , MessageTag
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DimensionValueSource = DimensionValueSource' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern EmailHeader :: DimensionValueSource
pattern EmailHeader = DimensionValueSource' "emailHeader"

pattern LinkTag :: DimensionValueSource
pattern LinkTag = DimensionValueSource' "linkTag"

pattern MessageTag :: DimensionValueSource
pattern MessageTag = DimensionValueSource' "messageTag"

{-# COMPLETE
  EmailHeader,
  LinkTag,
  MessageTag,
  DimensionValueSource' #-}

instance FromText DimensionValueSource where
    parser = (DimensionValueSource' . mk) <$> takeText

instance ToText DimensionValueSource where
    toText (DimensionValueSource' ci) = original ci

-- | Represents an enum of /known/ $DimensionValueSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DimensionValueSource where
    toEnum i = case i of
        0 -> EmailHeader
        1 -> LinkTag
        2 -> MessageTag
        _ -> (error . showText) $ "Unknown index for DimensionValueSource: " <> toText i
    fromEnum x = case x of
        EmailHeader -> 0
        LinkTag -> 1
        MessageTag -> 2
        DimensionValueSource' name -> (error . showText) $ "Unknown DimensionValueSource: " <> original name

-- | Represents the bounds of /known/ $DimensionValueSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DimensionValueSource where
    minBound = EmailHeader
    maxBound = MessageTag

instance Hashable     DimensionValueSource
instance NFData       DimensionValueSource
instance ToByteString DimensionValueSource
instance ToQuery      DimensionValueSource
instance ToHeader     DimensionValueSource

instance FromXML DimensionValueSource where
    parseXML = parseXMLText "DimensionValueSource"
