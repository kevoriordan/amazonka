{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSEncryptionSupport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EBSEncryptionSupport (
  EBSEncryptionSupport (
    ..
    , Supported
    , Unsupported
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EBSEncryptionSupport = EBSEncryptionSupport' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Supported :: EBSEncryptionSupport
pattern Supported = EBSEncryptionSupport' "supported"

pattern Unsupported :: EBSEncryptionSupport
pattern Unsupported = EBSEncryptionSupport' "unsupported"

{-# COMPLETE
  Supported,
  Unsupported,
  EBSEncryptionSupport' #-}

instance FromText EBSEncryptionSupport where
    parser = (EBSEncryptionSupport' . mk) <$> takeText

instance ToText EBSEncryptionSupport where
    toText (EBSEncryptionSupport' ci) = original ci

-- | Represents an enum of /known/ $EBSEncryptionSupport.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EBSEncryptionSupport where
    toEnum i = case i of
        0 -> Supported
        1 -> Unsupported
        _ -> (error . showText) $ "Unknown index for EBSEncryptionSupport: " <> toText i
    fromEnum x = case x of
        Supported -> 0
        Unsupported -> 1
        EBSEncryptionSupport' name -> (error . showText) $ "Unknown EBSEncryptionSupport: " <> original name

-- | Represents the bounds of /known/ $EBSEncryptionSupport.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EBSEncryptionSupport where
    minBound = Supported
    maxBound = Unsupported

instance Hashable     EBSEncryptionSupport
instance NFData       EBSEncryptionSupport
instance ToByteString EBSEncryptionSupport
instance ToQuery      EBSEncryptionSupport
instance ToHeader     EBSEncryptionSupport

instance FromXML EBSEncryptionSupport where
    parseXML = parseXMLText "EBSEncryptionSupport"
