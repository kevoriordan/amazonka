{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptFilterPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.ReceiptFilterPolicy (
  ReceiptFilterPolicy (
    ..
    , Allow
    , Block
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReceiptFilterPolicy = ReceiptFilterPolicy' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Allow :: ReceiptFilterPolicy
pattern Allow = ReceiptFilterPolicy' "Allow"

pattern Block :: ReceiptFilterPolicy
pattern Block = ReceiptFilterPolicy' "Block"

{-# COMPLETE
  Allow,
  Block,
  ReceiptFilterPolicy' #-}

instance FromText ReceiptFilterPolicy where
    parser = (ReceiptFilterPolicy' . mk) <$> takeText

instance ToText ReceiptFilterPolicy where
    toText (ReceiptFilterPolicy' ci) = original ci

-- | Represents an enum of /known/ $ReceiptFilterPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReceiptFilterPolicy where
    toEnum i = case i of
        0 -> Allow
        1 -> Block
        _ -> (error . showText) $ "Unknown index for ReceiptFilterPolicy: " <> toText i
    fromEnum x = case x of
        Allow -> 0
        Block -> 1
        ReceiptFilterPolicy' name -> (error . showText) $ "Unknown ReceiptFilterPolicy: " <> original name

-- | Represents the bounds of /known/ $ReceiptFilterPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReceiptFilterPolicy where
    minBound = Allow
    maxBound = Block

instance Hashable     ReceiptFilterPolicy
instance NFData       ReceiptFilterPolicy
instance ToByteString ReceiptFilterPolicy
instance ToQuery      ReceiptFilterPolicy
instance ToHeader     ReceiptFilterPolicy

instance FromXML ReceiptFilterPolicy where
    parseXML = parseXMLText "ReceiptFilterPolicy"
