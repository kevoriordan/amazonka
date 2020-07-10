{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Specialty
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.Specialty (
  Specialty (
    ..
    , Primarycare
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Specialty = Specialty' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Primarycare :: Specialty
pattern Primarycare = Specialty' "PRIMARYCARE"

{-# COMPLETE
  Primarycare,
  Specialty' #-}

instance FromText Specialty where
    parser = (Specialty' . mk) <$> takeText

instance ToText Specialty where
    toText (Specialty' ci) = original ci

-- | Represents an enum of /known/ $Specialty.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Specialty where
    toEnum i = case i of
        0 -> Primarycare
        _ -> (error . showText) $ "Unknown index for Specialty: " <> toText i
    fromEnum x = case x of
        Primarycare -> 0
        Specialty' name -> (error . showText) $ "Unknown Specialty: " <> original name

-- | Represents the bounds of /known/ $Specialty.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Specialty where
    minBound = Primarycare
    maxBound = Primarycare

instance Hashable     Specialty
instance NFData       Specialty
instance ToByteString Specialty
instance ToQuery      Specialty
instance ToHeader     Specialty

instance ToJSON Specialty where
    toJSON = toJSONText

instance FromJSON Specialty where
    parseJSON = parseJSONText "Specialty"
