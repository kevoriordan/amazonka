{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectVersionStorageClass
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectVersionStorageClass (
  ObjectVersionStorageClass (
    ..
    , OVSCStandard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectVersionStorageClass = ObjectVersionStorageClass' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern OVSCStandard :: ObjectVersionStorageClass
pattern OVSCStandard = ObjectVersionStorageClass' "STANDARD"

{-# COMPLETE
  OVSCStandard,
  ObjectVersionStorageClass' #-}

instance FromText ObjectVersionStorageClass where
    parser = (ObjectVersionStorageClass' . mk) <$> takeText

instance ToText ObjectVersionStorageClass where
    toText (ObjectVersionStorageClass' ci) = original ci

-- | Represents an enum of /known/ $ObjectVersionStorageClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObjectVersionStorageClass where
    toEnum i = case i of
        0 -> OVSCStandard
        _ -> (error . showText) $ "Unknown index for ObjectVersionStorageClass: " <> toText i
    fromEnum x = case x of
        OVSCStandard -> 0
        ObjectVersionStorageClass' name -> (error . showText) $ "Unknown ObjectVersionStorageClass: " <> original name

-- | Represents the bounds of /known/ $ObjectVersionStorageClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObjectVersionStorageClass where
    minBound = OVSCStandard
    maxBound = OVSCStandard

instance Hashable     ObjectVersionStorageClass
instance NFData       ObjectVersionStorageClass
instance ToByteString ObjectVersionStorageClass
instance ToQuery      ObjectVersionStorageClass
instance ToHeader     ObjectVersionStorageClass

instance FromXML ObjectVersionStorageClass where
    parseXML = parseXMLText "ObjectVersionStorageClass"
