{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Language
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Language (
  Language (
    ..
    , Python
    , Scala
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Language = Language' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Python :: Language
pattern Python = Language' "PYTHON"

pattern Scala :: Language
pattern Scala = Language' "SCALA"

{-# COMPLETE
  Python,
  Scala,
  Language' #-}

instance FromText Language where
    parser = (Language' . mk) <$> takeText

instance ToText Language where
    toText (Language' ci) = original ci

-- | Represents an enum of /known/ $Language.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Language where
    toEnum i = case i of
        0 -> Python
        1 -> Scala
        _ -> (error . showText) $ "Unknown index for Language: " <> toText i
    fromEnum x = case x of
        Python -> 0
        Scala -> 1
        Language' name -> (error . showText) $ "Unknown Language: " <> original name

-- | Represents the bounds of /known/ $Language.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Language where
    minBound = Python
    maxBound = Scala

instance Hashable     Language
instance NFData       Language
instance ToByteString Language
instance ToQuery      Language
instance ToHeader     Language

instance ToJSON Language where
    toJSON = toJSONText
