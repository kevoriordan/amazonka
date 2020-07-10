{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TerminologyDataFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.TerminologyDataFormat (
  TerminologyDataFormat (
    ..
    , CSV
    , Tmx
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TerminologyDataFormat = TerminologyDataFormat' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern CSV :: TerminologyDataFormat
pattern CSV = TerminologyDataFormat' "CSV"

pattern Tmx :: TerminologyDataFormat
pattern Tmx = TerminologyDataFormat' "TMX"

{-# COMPLETE
  CSV,
  Tmx,
  TerminologyDataFormat' #-}

instance FromText TerminologyDataFormat where
    parser = (TerminologyDataFormat' . mk) <$> takeText

instance ToText TerminologyDataFormat where
    toText (TerminologyDataFormat' ci) = original ci

-- | Represents an enum of /known/ $TerminologyDataFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TerminologyDataFormat where
    toEnum i = case i of
        0 -> CSV
        1 -> Tmx
        _ -> (error . showText) $ "Unknown index for TerminologyDataFormat: " <> toText i
    fromEnum x = case x of
        CSV -> 0
        Tmx -> 1
        TerminologyDataFormat' name -> (error . showText) $ "Unknown TerminologyDataFormat: " <> original name

-- | Represents the bounds of /known/ $TerminologyDataFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TerminologyDataFormat where
    minBound = CSV
    maxBound = Tmx

instance Hashable     TerminologyDataFormat
instance NFData       TerminologyDataFormat
instance ToByteString TerminologyDataFormat
instance ToQuery      TerminologyDataFormat
instance ToHeader     TerminologyDataFormat

instance ToJSON TerminologyDataFormat where
    toJSON = toJSONText
