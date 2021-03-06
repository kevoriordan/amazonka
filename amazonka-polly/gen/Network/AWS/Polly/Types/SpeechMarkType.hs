{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.SpeechMarkType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.SpeechMarkType (
  SpeechMarkType (
    ..
    , Sentence
    , Ssml
    , Viseme
    , Word
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SpeechMarkType = SpeechMarkType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Sentence :: SpeechMarkType
pattern Sentence = SpeechMarkType' "sentence"

pattern Ssml :: SpeechMarkType
pattern Ssml = SpeechMarkType' "ssml"

pattern Viseme :: SpeechMarkType
pattern Viseme = SpeechMarkType' "viseme"

pattern Word :: SpeechMarkType
pattern Word = SpeechMarkType' "word"

{-# COMPLETE
  Sentence,
  Ssml,
  Viseme,
  Word,
  SpeechMarkType' #-}

instance FromText SpeechMarkType where
    parser = (SpeechMarkType' . mk) <$> takeText

instance ToText SpeechMarkType where
    toText (SpeechMarkType' ci) = original ci

-- | Represents an enum of /known/ $SpeechMarkType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SpeechMarkType where
    toEnum i = case i of
        0 -> Sentence
        1 -> Ssml
        2 -> Viseme
        3 -> Word
        _ -> (error . showText) $ "Unknown index for SpeechMarkType: " <> toText i
    fromEnum x = case x of
        Sentence -> 0
        Ssml -> 1
        Viseme -> 2
        Word -> 3
        SpeechMarkType' name -> (error . showText) $ "Unknown SpeechMarkType: " <> original name

-- | Represents the bounds of /known/ $SpeechMarkType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SpeechMarkType where
    minBound = Sentence
    maxBound = Word

instance Hashable     SpeechMarkType
instance NFData       SpeechMarkType
instance ToByteString SpeechMarkType
instance ToQuery      SpeechMarkType
instance ToHeader     SpeechMarkType

instance ToJSON SpeechMarkType where
    toJSON = toJSONText

instance FromJSON SpeechMarkType where
    parseJSON = parseJSONText "SpeechMarkType"
