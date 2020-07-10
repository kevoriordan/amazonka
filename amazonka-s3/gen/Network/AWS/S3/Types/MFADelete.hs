{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MFADelete
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.MFADelete (
  MFADelete (
    ..
    , MDDisabled
    , MDEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data MFADelete = MFADelete' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern MDDisabled :: MFADelete
pattern MDDisabled = MFADelete' "Disabled"

pattern MDEnabled :: MFADelete
pattern MDEnabled = MFADelete' "Enabled"

{-# COMPLETE
  MDDisabled,
  MDEnabled,
  MFADelete' #-}

instance FromText MFADelete where
    parser = (MFADelete' . mk) <$> takeText

instance ToText MFADelete where
    toText (MFADelete' ci) = original ci

-- | Represents an enum of /known/ $MFADelete.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MFADelete where
    toEnum i = case i of
        0 -> MDDisabled
        1 -> MDEnabled
        _ -> (error . showText) $ "Unknown index for MFADelete: " <> toText i
    fromEnum x = case x of
        MDDisabled -> 0
        MDEnabled -> 1
        MFADelete' name -> (error . showText) $ "Unknown MFADelete: " <> original name

-- | Represents the bounds of /known/ $MFADelete.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MFADelete where
    minBound = MDDisabled
    maxBound = MDEnabled

instance Hashable     MFADelete
instance NFData       MFADelete
instance ToByteString MFADelete
instance ToQuery      MFADelete
instance ToHeader     MFADelete

instance ToXML MFADelete where
    toXML = toXMLText
