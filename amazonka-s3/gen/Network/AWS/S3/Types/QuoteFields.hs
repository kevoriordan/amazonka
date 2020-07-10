{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.QuoteFields
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.QuoteFields (
  QuoteFields (
    ..
    , ASNeeded
    , Always
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data QuoteFields = QuoteFields' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern ASNeeded :: QuoteFields
pattern ASNeeded = QuoteFields' "ASNEEDED"

pattern Always :: QuoteFields
pattern Always = QuoteFields' "ALWAYS"

{-# COMPLETE
  ASNeeded,
  Always,
  QuoteFields' #-}

instance FromText QuoteFields where
    parser = (QuoteFields' . mk) <$> takeText

instance ToText QuoteFields where
    toText (QuoteFields' ci) = original ci

-- | Represents an enum of /known/ $QuoteFields.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum QuoteFields where
    toEnum i = case i of
        0 -> ASNeeded
        1 -> Always
        _ -> (error . showText) $ "Unknown index for QuoteFields: " <> toText i
    fromEnum x = case x of
        ASNeeded -> 0
        Always -> 1
        QuoteFields' name -> (error . showText) $ "Unknown QuoteFields: " <> original name

-- | Represents the bounds of /known/ $QuoteFields.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded QuoteFields where
    minBound = ASNeeded
    maxBound = Always

instance Hashable     QuoteFields
instance NFData       QuoteFields
instance ToByteString QuoteFields
instance ToQuery      QuoteFields
instance ToHeader     QuoteFields

instance ToXML QuoteFields where
    toXML = toXMLText
