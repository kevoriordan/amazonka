{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.RedactionOutput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.RedactionOutput (
  RedactionOutput (
    ..
    , Redacted
    , RedactedAndUnredacted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RedactionOutput = RedactionOutput' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Redacted :: RedactionOutput
pattern Redacted = RedactionOutput' "redacted"

pattern RedactedAndUnredacted :: RedactionOutput
pattern RedactedAndUnredacted = RedactionOutput' "redacted_and_unredacted"

{-# COMPLETE
  Redacted,
  RedactedAndUnredacted,
  RedactionOutput' #-}

instance FromText RedactionOutput where
    parser = (RedactionOutput' . mk) <$> takeText

instance ToText RedactionOutput where
    toText (RedactionOutput' ci) = original ci

-- | Represents an enum of /known/ $RedactionOutput.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RedactionOutput where
    toEnum i = case i of
        0 -> Redacted
        1 -> RedactedAndUnredacted
        _ -> (error . showText) $ "Unknown index for RedactionOutput: " <> toText i
    fromEnum x = case x of
        Redacted -> 0
        RedactedAndUnredacted -> 1
        RedactionOutput' name -> (error . showText) $ "Unknown RedactionOutput: " <> original name

-- | Represents the bounds of /known/ $RedactionOutput.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RedactionOutput where
    minBound = Redacted
    maxBound = RedactedAndUnredacted

instance Hashable     RedactionOutput
instance NFData       RedactionOutput
instance ToByteString RedactionOutput
instance ToQuery      RedactionOutput
instance ToHeader     RedactionOutput

instance ToJSON RedactionOutput where
    toJSON = toJSONText

instance FromJSON RedactionOutput where
    parseJSON = parseJSONText "RedactionOutput"
