{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RevisionLocationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.RevisionLocationType (
  RevisionLocationType (
    ..
    , AppSpecContent
    , GitHub
    , S3
    , String
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RevisionLocationType = RevisionLocationType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern AppSpecContent :: RevisionLocationType
pattern AppSpecContent = RevisionLocationType' "AppSpecContent"

pattern GitHub :: RevisionLocationType
pattern GitHub = RevisionLocationType' "GitHub"

pattern S3 :: RevisionLocationType
pattern S3 = RevisionLocationType' "S3"

pattern String :: RevisionLocationType
pattern String = RevisionLocationType' "String"

{-# COMPLETE
  AppSpecContent,
  GitHub,
  S3,
  String,
  RevisionLocationType' #-}

instance FromText RevisionLocationType where
    parser = (RevisionLocationType' . mk) <$> takeText

instance ToText RevisionLocationType where
    toText (RevisionLocationType' ci) = original ci

-- | Represents an enum of /known/ $RevisionLocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RevisionLocationType where
    toEnum i = case i of
        0 -> AppSpecContent
        1 -> GitHub
        2 -> S3
        3 -> String
        _ -> (error . showText) $ "Unknown index for RevisionLocationType: " <> toText i
    fromEnum x = case x of
        AppSpecContent -> 0
        GitHub -> 1
        S3 -> 2
        String -> 3
        RevisionLocationType' name -> (error . showText) $ "Unknown RevisionLocationType: " <> original name

-- | Represents the bounds of /known/ $RevisionLocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RevisionLocationType where
    minBound = AppSpecContent
    maxBound = String

instance Hashable     RevisionLocationType
instance NFData       RevisionLocationType
instance ToByteString RevisionLocationType
instance ToQuery      RevisionLocationType
instance ToHeader     RevisionLocationType

instance ToJSON RevisionLocationType where
    toJSON = toJSONText

instance FromJSON RevisionLocationType where
    parseJSON = parseJSONText "RevisionLocationType"
