{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.BillingMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.BillingMethod (
  BillingMethod (
    ..
    , Metered
    , Unmetered
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BillingMethod = BillingMethod' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Metered :: BillingMethod
pattern Metered = BillingMethod' "METERED"

pattern Unmetered :: BillingMethod
pattern Unmetered = BillingMethod' "UNMETERED"

{-# COMPLETE
  Metered,
  Unmetered,
  BillingMethod' #-}

instance FromText BillingMethod where
    parser = (BillingMethod' . mk) <$> takeText

instance ToText BillingMethod where
    toText (BillingMethod' ci) = original ci

-- | Represents an enum of /known/ $BillingMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BillingMethod where
    toEnum i = case i of
        0 -> Metered
        1 -> Unmetered
        _ -> (error . showText) $ "Unknown index for BillingMethod: " <> toText i
    fromEnum x = case x of
        Metered -> 0
        Unmetered -> 1
        BillingMethod' name -> (error . showText) $ "Unknown BillingMethod: " <> original name

-- | Represents the bounds of /known/ $BillingMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BillingMethod where
    minBound = Metered
    maxBound = Unmetered

instance Hashable     BillingMethod
instance NFData       BillingMethod
instance ToByteString BillingMethod
instance ToQuery      BillingMethod
instance ToHeader     BillingMethod

instance ToJSON BillingMethod where
    toJSON = toJSONText

instance FromJSON BillingMethod where
    parseJSON = parseJSONText "BillingMethod"
