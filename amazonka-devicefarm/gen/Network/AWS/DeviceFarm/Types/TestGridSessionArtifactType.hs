{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType (
  TestGridSessionArtifactType (
    ..
    , SeleniumLog
    , Unknown
    , Video
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TestGridSessionArtifactType = TestGridSessionArtifactType' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern SeleniumLog :: TestGridSessionArtifactType
pattern SeleniumLog = TestGridSessionArtifactType' "SELENIUM_LOG"

pattern Unknown :: TestGridSessionArtifactType
pattern Unknown = TestGridSessionArtifactType' "UNKNOWN"

pattern Video :: TestGridSessionArtifactType
pattern Video = TestGridSessionArtifactType' "VIDEO"

{-# COMPLETE
  SeleniumLog,
  Unknown,
  Video,
  TestGridSessionArtifactType' #-}

instance FromText TestGridSessionArtifactType where
    parser = (TestGridSessionArtifactType' . mk) <$> takeText

instance ToText TestGridSessionArtifactType where
    toText (TestGridSessionArtifactType' ci) = original ci

-- | Represents an enum of /known/ $TestGridSessionArtifactType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TestGridSessionArtifactType where
    toEnum i = case i of
        0 -> SeleniumLog
        1 -> Unknown
        2 -> Video
        _ -> (error . showText) $ "Unknown index for TestGridSessionArtifactType: " <> toText i
    fromEnum x = case x of
        SeleniumLog -> 0
        Unknown -> 1
        Video -> 2
        TestGridSessionArtifactType' name -> (error . showText) $ "Unknown TestGridSessionArtifactType: " <> original name

-- | Represents the bounds of /known/ $TestGridSessionArtifactType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TestGridSessionArtifactType where
    minBound = SeleniumLog
    maxBound = Video

instance Hashable     TestGridSessionArtifactType
instance NFData       TestGridSessionArtifactType
instance ToByteString TestGridSessionArtifactType
instance ToQuery      TestGridSessionArtifactType
instance ToHeader     TestGridSessionArtifactType

instance FromJSON TestGridSessionArtifactType where
    parseJSON = parseJSONText "TestGridSessionArtifactType"
