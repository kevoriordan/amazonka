{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior (
  Fmp4NielsenId3Behavior (
    ..
    , FNIBNoPassthrough
    , FNIBPassthrough
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Fmp4 Nielsen Id3 Behavior
data Fmp4NielsenId3Behavior = Fmp4NielsenId3Behavior' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern FNIBNoPassthrough :: Fmp4NielsenId3Behavior
pattern FNIBNoPassthrough = Fmp4NielsenId3Behavior' "NO_PASSTHROUGH"

pattern FNIBPassthrough :: Fmp4NielsenId3Behavior
pattern FNIBPassthrough = Fmp4NielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE
  FNIBNoPassthrough,
  FNIBPassthrough,
  Fmp4NielsenId3Behavior' #-}

instance FromText Fmp4NielsenId3Behavior where
    parser = (Fmp4NielsenId3Behavior' . mk) <$> takeText

instance ToText Fmp4NielsenId3Behavior where
    toText (Fmp4NielsenId3Behavior' ci) = original ci

-- | Represents an enum of /known/ $Fmp4NielsenId3Behavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Fmp4NielsenId3Behavior where
    toEnum i = case i of
        0 -> FNIBNoPassthrough
        1 -> FNIBPassthrough
        _ -> (error . showText) $ "Unknown index for Fmp4NielsenId3Behavior: " <> toText i
    fromEnum x = case x of
        FNIBNoPassthrough -> 0
        FNIBPassthrough -> 1
        Fmp4NielsenId3Behavior' name -> (error . showText) $ "Unknown Fmp4NielsenId3Behavior: " <> original name

-- | Represents the bounds of /known/ $Fmp4NielsenId3Behavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Fmp4NielsenId3Behavior where
    minBound = FNIBNoPassthrough
    maxBound = FNIBPassthrough

instance Hashable     Fmp4NielsenId3Behavior
instance NFData       Fmp4NielsenId3Behavior
instance ToByteString Fmp4NielsenId3Behavior
instance ToQuery      Fmp4NielsenId3Behavior
instance ToHeader     Fmp4NielsenId3Behavior

instance ToJSON Fmp4NielsenId3Behavior where
    toJSON = toJSONText

instance FromJSON Fmp4NielsenId3Behavior where
    parseJSON = parseJSONText "Fmp4NielsenId3Behavior"
