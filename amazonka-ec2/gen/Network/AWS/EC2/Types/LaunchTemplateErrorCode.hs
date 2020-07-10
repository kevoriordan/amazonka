{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateErrorCode (
  LaunchTemplateErrorCode (
    ..
    , LaunchTemplateIdDoesNotExist
    , LaunchTemplateIdMalformed
    , LaunchTemplateNameDoesNotExist
    , LaunchTemplateNameMalformed
    , LaunchTemplateVersionDoesNotExist
    , UnexpectedError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LaunchTemplateErrorCode = LaunchTemplateErrorCode' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern LaunchTemplateIdDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateIdDoesNotExist = LaunchTemplateErrorCode' "launchTemplateIdDoesNotExist"

pattern LaunchTemplateIdMalformed :: LaunchTemplateErrorCode
pattern LaunchTemplateIdMalformed = LaunchTemplateErrorCode' "launchTemplateIdMalformed"

pattern LaunchTemplateNameDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateNameDoesNotExist = LaunchTemplateErrorCode' "launchTemplateNameDoesNotExist"

pattern LaunchTemplateNameMalformed :: LaunchTemplateErrorCode
pattern LaunchTemplateNameMalformed = LaunchTemplateErrorCode' "launchTemplateNameMalformed"

pattern LaunchTemplateVersionDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateVersionDoesNotExist = LaunchTemplateErrorCode' "launchTemplateVersionDoesNotExist"

pattern UnexpectedError :: LaunchTemplateErrorCode
pattern UnexpectedError = LaunchTemplateErrorCode' "unexpectedError"

{-# COMPLETE
  LaunchTemplateIdDoesNotExist,
  LaunchTemplateIdMalformed,
  LaunchTemplateNameDoesNotExist,
  LaunchTemplateNameMalformed,
  LaunchTemplateVersionDoesNotExist,
  UnexpectedError,
  LaunchTemplateErrorCode' #-}

instance FromText LaunchTemplateErrorCode where
    parser = (LaunchTemplateErrorCode' . mk) <$> takeText

instance ToText LaunchTemplateErrorCode where
    toText (LaunchTemplateErrorCode' ci) = original ci

-- | Represents an enum of /known/ $LaunchTemplateErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LaunchTemplateErrorCode where
    toEnum i = case i of
        0 -> LaunchTemplateIdDoesNotExist
        1 -> LaunchTemplateIdMalformed
        2 -> LaunchTemplateNameDoesNotExist
        3 -> LaunchTemplateNameMalformed
        4 -> LaunchTemplateVersionDoesNotExist
        5 -> UnexpectedError
        _ -> (error . showText) $ "Unknown index for LaunchTemplateErrorCode: " <> toText i
    fromEnum x = case x of
        LaunchTemplateIdDoesNotExist -> 0
        LaunchTemplateIdMalformed -> 1
        LaunchTemplateNameDoesNotExist -> 2
        LaunchTemplateNameMalformed -> 3
        LaunchTemplateVersionDoesNotExist -> 4
        UnexpectedError -> 5
        LaunchTemplateErrorCode' name -> (error . showText) $ "Unknown LaunchTemplateErrorCode: " <> original name

-- | Represents the bounds of /known/ $LaunchTemplateErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LaunchTemplateErrorCode where
    minBound = LaunchTemplateIdDoesNotExist
    maxBound = UnexpectedError

instance Hashable     LaunchTemplateErrorCode
instance NFData       LaunchTemplateErrorCode
instance ToByteString LaunchTemplateErrorCode
instance ToQuery      LaunchTemplateErrorCode
instance ToHeader     LaunchTemplateErrorCode

instance FromXML LaunchTemplateErrorCode where
    parseXML = parseXMLText "LaunchTemplateErrorCode"
