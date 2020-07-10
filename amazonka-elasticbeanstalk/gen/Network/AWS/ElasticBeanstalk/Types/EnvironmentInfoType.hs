{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoType (
  EnvironmentInfoType (
    ..
    , Bundle
    , Tail
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnvironmentInfoType = EnvironmentInfoType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Bundle :: EnvironmentInfoType
pattern Bundle = EnvironmentInfoType' "bundle"

pattern Tail :: EnvironmentInfoType
pattern Tail = EnvironmentInfoType' "tail"

{-# COMPLETE
  Bundle,
  Tail,
  EnvironmentInfoType' #-}

instance FromText EnvironmentInfoType where
    parser = (EnvironmentInfoType' . mk) <$> takeText

instance ToText EnvironmentInfoType where
    toText (EnvironmentInfoType' ci) = original ci

-- | Represents an enum of /known/ $EnvironmentInfoType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EnvironmentInfoType where
    toEnum i = case i of
        0 -> Bundle
        1 -> Tail
        _ -> (error . showText) $ "Unknown index for EnvironmentInfoType: " <> toText i
    fromEnum x = case x of
        Bundle -> 0
        Tail -> 1
        EnvironmentInfoType' name -> (error . showText) $ "Unknown EnvironmentInfoType: " <> original name

-- | Represents the bounds of /known/ $EnvironmentInfoType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnvironmentInfoType where
    minBound = Bundle
    maxBound = Tail

instance Hashable     EnvironmentInfoType
instance NFData       EnvironmentInfoType
instance ToByteString EnvironmentInfoType
instance ToQuery      EnvironmentInfoType
instance ToHeader     EnvironmentInfoType

instance FromXML EnvironmentInfoType where
    parseXML = parseXMLText "EnvironmentInfoType"
