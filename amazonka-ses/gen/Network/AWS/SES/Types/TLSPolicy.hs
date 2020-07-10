{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.TLSPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.TLSPolicy (
  TLSPolicy (
    ..
    , Optional
    , Require
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TLSPolicy = TLSPolicy' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Optional :: TLSPolicy
pattern Optional = TLSPolicy' "Optional"

pattern Require :: TLSPolicy
pattern Require = TLSPolicy' "Require"

{-# COMPLETE
  Optional,
  Require,
  TLSPolicy' #-}

instance FromText TLSPolicy where
    parser = (TLSPolicy' . mk) <$> takeText

instance ToText TLSPolicy where
    toText (TLSPolicy' ci) = original ci

-- | Represents an enum of /known/ $TLSPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TLSPolicy where
    toEnum i = case i of
        0 -> Optional
        1 -> Require
        _ -> (error . showText) $ "Unknown index for TLSPolicy: " <> toText i
    fromEnum x = case x of
        Optional -> 0
        Require -> 1
        TLSPolicy' name -> (error . showText) $ "Unknown TLSPolicy: " <> original name

-- | Represents the bounds of /known/ $TLSPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TLSPolicy where
    minBound = Optional
    maxBound = Require

instance Hashable     TLSPolicy
instance NFData       TLSPolicy
instance ToByteString TLSPolicy
instance ToQuery      TLSPolicy
instance ToHeader     TLSPolicy

instance FromXML TLSPolicy where
    parseXML = parseXMLText "TLSPolicy"
