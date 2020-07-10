{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportEnvironment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ExportEnvironment (
  ExportEnvironment (
    ..
    , Citrix
    , Microsoft
    , VMware
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ExportEnvironment = ExportEnvironment' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Citrix :: ExportEnvironment
pattern Citrix = ExportEnvironment' "citrix"

pattern Microsoft :: ExportEnvironment
pattern Microsoft = ExportEnvironment' "microsoft"

pattern VMware :: ExportEnvironment
pattern VMware = ExportEnvironment' "vmware"

{-# COMPLETE
  Citrix,
  Microsoft,
  VMware,
  ExportEnvironment' #-}

instance FromText ExportEnvironment where
    parser = (ExportEnvironment' . mk) <$> takeText

instance ToText ExportEnvironment where
    toText (ExportEnvironment' ci) = original ci

-- | Represents an enum of /known/ $ExportEnvironment.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExportEnvironment where
    toEnum i = case i of
        0 -> Citrix
        1 -> Microsoft
        2 -> VMware
        _ -> (error . showText) $ "Unknown index for ExportEnvironment: " <> toText i
    fromEnum x = case x of
        Citrix -> 0
        Microsoft -> 1
        VMware -> 2
        ExportEnvironment' name -> (error . showText) $ "Unknown ExportEnvironment: " <> original name

-- | Represents the bounds of /known/ $ExportEnvironment.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExportEnvironment where
    minBound = Citrix
    maxBound = VMware

instance Hashable     ExportEnvironment
instance NFData       ExportEnvironment
instance ToByteString ExportEnvironment
instance ToQuery      ExportEnvironment
instance ToHeader     ExportEnvironment

instance FromXML ExportEnvironment where
    parseXML = parseXMLText "ExportEnvironment"
