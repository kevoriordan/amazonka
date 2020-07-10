{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PaymentOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PaymentOption (
  PaymentOption (
    ..
    , POAllUpfront
    , PONoUpfront
    , POPartialUpfront
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PaymentOption = PaymentOption' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern POAllUpfront :: PaymentOption
pattern POAllUpfront = PaymentOption' "AllUpfront"

pattern PONoUpfront :: PaymentOption
pattern PONoUpfront = PaymentOption' "NoUpfront"

pattern POPartialUpfront :: PaymentOption
pattern POPartialUpfront = PaymentOption' "PartialUpfront"

{-# COMPLETE
  POAllUpfront,
  PONoUpfront,
  POPartialUpfront,
  PaymentOption' #-}

instance FromText PaymentOption where
    parser = (PaymentOption' . mk) <$> takeText

instance ToText PaymentOption where
    toText (PaymentOption' ci) = original ci

-- | Represents an enum of /known/ $PaymentOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PaymentOption where
    toEnum i = case i of
        0 -> POAllUpfront
        1 -> PONoUpfront
        2 -> POPartialUpfront
        _ -> (error . showText) $ "Unknown index for PaymentOption: " <> toText i
    fromEnum x = case x of
        POAllUpfront -> 0
        PONoUpfront -> 1
        POPartialUpfront -> 2
        PaymentOption' name -> (error . showText) $ "Unknown PaymentOption: " <> original name

-- | Represents the bounds of /known/ $PaymentOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PaymentOption where
    minBound = POAllUpfront
    maxBound = POPartialUpfront

instance Hashable     PaymentOption
instance NFData       PaymentOption
instance ToByteString PaymentOption
instance ToQuery      PaymentOption
instance ToHeader     PaymentOption

instance FromXML PaymentOption where
    parseXML = parseXMLText "PaymentOption"
