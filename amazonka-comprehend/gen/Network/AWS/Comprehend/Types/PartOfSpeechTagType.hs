{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PartOfSpeechTagType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.PartOfSpeechTagType (
  PartOfSpeechTagType (
    ..
    , Adj
    , Adp
    , Adv
    , Aux
    , Cconj
    , Conj
    , Det
    , Intj
    , Noun
    , Num
    , O
    , Part
    , Pron
    , Propn
    , Punct
    , Sconj
    , Sym
    , Verb
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PartOfSpeechTagType = PartOfSpeechTagType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Adj :: PartOfSpeechTagType
pattern Adj = PartOfSpeechTagType' "ADJ"

pattern Adp :: PartOfSpeechTagType
pattern Adp = PartOfSpeechTagType' "ADP"

pattern Adv :: PartOfSpeechTagType
pattern Adv = PartOfSpeechTagType' "ADV"

pattern Aux :: PartOfSpeechTagType
pattern Aux = PartOfSpeechTagType' "AUX"

pattern Cconj :: PartOfSpeechTagType
pattern Cconj = PartOfSpeechTagType' "CCONJ"

pattern Conj :: PartOfSpeechTagType
pattern Conj = PartOfSpeechTagType' "CONJ"

pattern Det :: PartOfSpeechTagType
pattern Det = PartOfSpeechTagType' "DET"

pattern Intj :: PartOfSpeechTagType
pattern Intj = PartOfSpeechTagType' "INTJ"

pattern Noun :: PartOfSpeechTagType
pattern Noun = PartOfSpeechTagType' "NOUN"

pattern Num :: PartOfSpeechTagType
pattern Num = PartOfSpeechTagType' "NUM"

pattern O :: PartOfSpeechTagType
pattern O = PartOfSpeechTagType' "O"

pattern Part :: PartOfSpeechTagType
pattern Part = PartOfSpeechTagType' "PART"

pattern Pron :: PartOfSpeechTagType
pattern Pron = PartOfSpeechTagType' "PRON"

pattern Propn :: PartOfSpeechTagType
pattern Propn = PartOfSpeechTagType' "PROPN"

pattern Punct :: PartOfSpeechTagType
pattern Punct = PartOfSpeechTagType' "PUNCT"

pattern Sconj :: PartOfSpeechTagType
pattern Sconj = PartOfSpeechTagType' "SCONJ"

pattern Sym :: PartOfSpeechTagType
pattern Sym = PartOfSpeechTagType' "SYM"

pattern Verb :: PartOfSpeechTagType
pattern Verb = PartOfSpeechTagType' "VERB"

{-# COMPLETE
  Adj,
  Adp,
  Adv,
  Aux,
  Cconj,
  Conj,
  Det,
  Intj,
  Noun,
  Num,
  O,
  Part,
  Pron,
  Propn,
  Punct,
  Sconj,
  Sym,
  Verb,
  PartOfSpeechTagType' #-}

instance FromText PartOfSpeechTagType where
    parser = (PartOfSpeechTagType' . mk) <$> takeText

instance ToText PartOfSpeechTagType where
    toText (PartOfSpeechTagType' ci) = original ci

-- | Represents an enum of /known/ $PartOfSpeechTagType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PartOfSpeechTagType where
    toEnum i = case i of
        0 -> Adj
        1 -> Adp
        2 -> Adv
        3 -> Aux
        4 -> Cconj
        5 -> Conj
        6 -> Det
        7 -> Intj
        8 -> Noun
        9 -> Num
        10 -> O
        11 -> Part
        12 -> Pron
        13 -> Propn
        14 -> Punct
        15 -> Sconj
        16 -> Sym
        17 -> Verb
        _ -> (error . showText) $ "Unknown index for PartOfSpeechTagType: " <> toText i
    fromEnum x = case x of
        Adj -> 0
        Adp -> 1
        Adv -> 2
        Aux -> 3
        Cconj -> 4
        Conj -> 5
        Det -> 6
        Intj -> 7
        Noun -> 8
        Num -> 9
        O -> 10
        Part -> 11
        Pron -> 12
        Propn -> 13
        Punct -> 14
        Sconj -> 15
        Sym -> 16
        Verb -> 17
        PartOfSpeechTagType' name -> (error . showText) $ "Unknown PartOfSpeechTagType: " <> original name

-- | Represents the bounds of /known/ $PartOfSpeechTagType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PartOfSpeechTagType where
    minBound = Adj
    maxBound = Verb

instance Hashable     PartOfSpeechTagType
instance NFData       PartOfSpeechTagType
instance ToByteString PartOfSpeechTagType
instance ToQuery      PartOfSpeechTagType
instance ToHeader     PartOfSpeechTagType

instance FromJSON PartOfSpeechTagType where
    parseJSON = parseJSONText "PartOfSpeechTagType"
