{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ChangeSource (
  ChangeSource (
    ..
    , Automatic
    , DirectModification
    , ParameterReference
    , ResourceAttribute
    , ResourceReference
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChangeSource = ChangeSource' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Automatic :: ChangeSource
pattern Automatic = ChangeSource' "Automatic"

pattern DirectModification :: ChangeSource
pattern DirectModification = ChangeSource' "DirectModification"

pattern ParameterReference :: ChangeSource
pattern ParameterReference = ChangeSource' "ParameterReference"

pattern ResourceAttribute :: ChangeSource
pattern ResourceAttribute = ChangeSource' "ResourceAttribute"

pattern ResourceReference :: ChangeSource
pattern ResourceReference = ChangeSource' "ResourceReference"

{-# COMPLETE
  Automatic,
  DirectModification,
  ParameterReference,
  ResourceAttribute,
  ResourceReference,
  ChangeSource' #-}

instance FromText ChangeSource where
    parser = (ChangeSource' . mk) <$> takeText

instance ToText ChangeSource where
    toText (ChangeSource' ci) = original ci

-- | Represents an enum of /known/ $ChangeSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeSource where
    toEnum i = case i of
        0 -> Automatic
        1 -> DirectModification
        2 -> ParameterReference
        3 -> ResourceAttribute
        4 -> ResourceReference
        _ -> (error . showText) $ "Unknown index for ChangeSource: " <> toText i
    fromEnum x = case x of
        Automatic -> 0
        DirectModification -> 1
        ParameterReference -> 2
        ResourceAttribute -> 3
        ResourceReference -> 4
        ChangeSource' name -> (error . showText) $ "Unknown ChangeSource: " <> original name

-- | Represents the bounds of /known/ $ChangeSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeSource where
    minBound = Automatic
    maxBound = ResourceReference

instance Hashable     ChangeSource
instance NFData       ChangeSource
instance ToByteString ChangeSource
instance ToQuery      ChangeSource
instance ToHeader     ChangeSource

instance FromXML ChangeSource where
    parseXML = parseXMLText "ChangeSource"
