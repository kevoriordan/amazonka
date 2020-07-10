{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.VocabularyFilterMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.VocabularyFilterMethod (
  VocabularyFilterMethod (
    ..
    , Mask
    , Remove
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data VocabularyFilterMethod = VocabularyFilterMethod' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Mask :: VocabularyFilterMethod
pattern Mask = VocabularyFilterMethod' "mask"

pattern Remove :: VocabularyFilterMethod
pattern Remove = VocabularyFilterMethod' "remove"

{-# COMPLETE
  Mask,
  Remove,
  VocabularyFilterMethod' #-}

instance FromText VocabularyFilterMethod where
    parser = (VocabularyFilterMethod' . mk) <$> takeText

instance ToText VocabularyFilterMethod where
    toText (VocabularyFilterMethod' ci) = original ci

-- | Represents an enum of /known/ $VocabularyFilterMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VocabularyFilterMethod where
    toEnum i = case i of
        0 -> Mask
        1 -> Remove
        _ -> (error . showText) $ "Unknown index for VocabularyFilterMethod: " <> toText i
    fromEnum x = case x of
        Mask -> 0
        Remove -> 1
        VocabularyFilterMethod' name -> (error . showText) $ "Unknown VocabularyFilterMethod: " <> original name

-- | Represents the bounds of /known/ $VocabularyFilterMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VocabularyFilterMethod where
    minBound = Mask
    maxBound = Remove

instance Hashable     VocabularyFilterMethod
instance NFData       VocabularyFilterMethod
instance ToByteString VocabularyFilterMethod
instance ToQuery      VocabularyFilterMethod
instance ToHeader     VocabularyFilterMethod

instance ToJSON VocabularyFilterMethod where
    toJSON = toJSONText

instance FromJSON VocabularyFilterMethod where
    parseJSON = parseJSONText "VocabularyFilterMethod"
