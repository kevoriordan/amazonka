{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.RedactionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.RedactionType (
  RedactionType (
    ..
    , Pii
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RedactionType = RedactionType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Pii :: RedactionType
pattern Pii = RedactionType' "PII"

{-# COMPLETE
  Pii,
  RedactionType' #-}

instance FromText RedactionType where
    parser = (RedactionType' . mk) <$> takeText

instance ToText RedactionType where
    toText (RedactionType' ci) = original ci

-- | Represents an enum of /known/ $RedactionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RedactionType where
    toEnum i = case i of
        0 -> Pii
        _ -> (error . showText) $ "Unknown index for RedactionType: " <> toText i
    fromEnum x = case x of
        Pii -> 0
        RedactionType' name -> (error . showText) $ "Unknown RedactionType: " <> original name

-- | Represents the bounds of /known/ $RedactionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RedactionType where
    minBound = Pii
    maxBound = Pii

instance Hashable     RedactionType
instance NFData       RedactionType
instance ToByteString RedactionType
instance ToQuery      RedactionType
instance ToHeader     RedactionType

instance ToJSON RedactionType where
    toJSON = toJSONText

instance FromJSON RedactionType where
    parseJSON = parseJSONText "RedactionType"
