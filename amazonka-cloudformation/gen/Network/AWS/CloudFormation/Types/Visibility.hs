{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Visibility
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Visibility (
  Visibility (
    ..
    , Private
    , Public
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Visibility = Visibility' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Private :: Visibility
pattern Private = Visibility' "PRIVATE"

pattern Public :: Visibility
pattern Public = Visibility' "PUBLIC"

{-# COMPLETE
  Private,
  Public,
  Visibility' #-}

instance FromText Visibility where
    parser = (Visibility' . mk) <$> takeText

instance ToText Visibility where
    toText (Visibility' ci) = original ci

-- | Represents an enum of /known/ $Visibility.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Visibility where
    toEnum i = case i of
        0 -> Private
        1 -> Public
        _ -> (error . showText) $ "Unknown index for Visibility: " <> toText i
    fromEnum x = case x of
        Private -> 0
        Public -> 1
        Visibility' name -> (error . showText) $ "Unknown Visibility: " <> original name

-- | Represents the bounds of /known/ $Visibility.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Visibility where
    minBound = Private
    maxBound = Public

instance Hashable     Visibility
instance NFData       Visibility
instance ToByteString Visibility
instance ToQuery      Visibility
instance ToHeader     Visibility

instance FromXML Visibility where
    parseXML = parseXMLText "Visibility"
