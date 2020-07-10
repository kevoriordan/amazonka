{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.Replacement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.Replacement (
  Replacement (
    ..
    , Conditional
    , False'
    , True'
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Replacement = Replacement' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Conditional :: Replacement
pattern Conditional = Replacement' "CONDITIONAL"

pattern False' :: Replacement
pattern False' = Replacement' "FALSE"

pattern True' :: Replacement
pattern True' = Replacement' "TRUE"

{-# COMPLETE
  Conditional,
  False',
  True',
  Replacement' #-}

instance FromText Replacement where
    parser = (Replacement' . mk) <$> takeText

instance ToText Replacement where
    toText (Replacement' ci) = original ci

-- | Represents an enum of /known/ $Replacement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Replacement where
    toEnum i = case i of
        0 -> Conditional
        1 -> False'
        2 -> True'
        _ -> (error . showText) $ "Unknown index for Replacement: " <> toText i
    fromEnum x = case x of
        Conditional -> 0
        False' -> 1
        True' -> 2
        Replacement' name -> (error . showText) $ "Unknown Replacement: " <> original name

-- | Represents the bounds of /known/ $Replacement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Replacement where
    minBound = Conditional
    maxBound = True'

instance Hashable     Replacement
instance NFData       Replacement
instance ToByteString Replacement
instance ToQuery      Replacement
instance ToHeader     Replacement

instance FromJSON Replacement where
    parseJSON = parseJSONText "Replacement"
