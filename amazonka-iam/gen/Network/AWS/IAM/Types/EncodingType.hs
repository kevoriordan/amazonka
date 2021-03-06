{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EncodingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.EncodingType (
  EncodingType (
    ..
    , Pem
    , SSH
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EncodingType = EncodingType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Pem :: EncodingType
pattern Pem = EncodingType' "PEM"

pattern SSH :: EncodingType
pattern SSH = EncodingType' "SSH"

{-# COMPLETE
  Pem,
  SSH,
  EncodingType' #-}

instance FromText EncodingType where
    parser = (EncodingType' . mk) <$> takeText

instance ToText EncodingType where
    toText (EncodingType' ci) = original ci

-- | Represents an enum of /known/ $EncodingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EncodingType where
    toEnum i = case i of
        0 -> Pem
        1 -> SSH
        _ -> (error . showText) $ "Unknown index for EncodingType: " <> toText i
    fromEnum x = case x of
        Pem -> 0
        SSH -> 1
        EncodingType' name -> (error . showText) $ "Unknown EncodingType: " <> original name

-- | Represents the bounds of /known/ $EncodingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EncodingType where
    minBound = Pem
    maxBound = SSH

instance Hashable     EncodingType
instance NFData       EncodingType
instance ToByteString EncodingType
instance ToQuery      EncodingType
instance ToHeader     EncodingType
