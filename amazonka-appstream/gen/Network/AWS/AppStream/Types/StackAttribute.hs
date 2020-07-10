{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StackAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.StackAttribute (
  StackAttribute (
    ..
    , AccessEndpoints
    , EmbedHostDomains
    , FeedbackURL
    , IAMRoleARN
    , RedirectURL
    , StorageConnectorGoogleDrive
    , StorageConnectorHomefolders
    , StorageConnectorOneDrive
    , StorageConnectors
    , ThemeName
    , UserSettings
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackAttribute = StackAttribute' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern AccessEndpoints :: StackAttribute
pattern AccessEndpoints = StackAttribute' "ACCESS_ENDPOINTS"

pattern EmbedHostDomains :: StackAttribute
pattern EmbedHostDomains = StackAttribute' "EMBED_HOST_DOMAINS"

pattern FeedbackURL :: StackAttribute
pattern FeedbackURL = StackAttribute' "FEEDBACK_URL"

pattern IAMRoleARN :: StackAttribute
pattern IAMRoleARN = StackAttribute' "IAM_ROLE_ARN"

pattern RedirectURL :: StackAttribute
pattern RedirectURL = StackAttribute' "REDIRECT_URL"

pattern StorageConnectorGoogleDrive :: StackAttribute
pattern StorageConnectorGoogleDrive = StackAttribute' "STORAGE_CONNECTOR_GOOGLE_DRIVE"

pattern StorageConnectorHomefolders :: StackAttribute
pattern StorageConnectorHomefolders = StackAttribute' "STORAGE_CONNECTOR_HOMEFOLDERS"

pattern StorageConnectorOneDrive :: StackAttribute
pattern StorageConnectorOneDrive = StackAttribute' "STORAGE_CONNECTOR_ONE_DRIVE"

pattern StorageConnectors :: StackAttribute
pattern StorageConnectors = StackAttribute' "STORAGE_CONNECTORS"

pattern ThemeName :: StackAttribute
pattern ThemeName = StackAttribute' "THEME_NAME"

pattern UserSettings :: StackAttribute
pattern UserSettings = StackAttribute' "USER_SETTINGS"

{-# COMPLETE
  AccessEndpoints,
  EmbedHostDomains,
  FeedbackURL,
  IAMRoleARN,
  RedirectURL,
  StorageConnectorGoogleDrive,
  StorageConnectorHomefolders,
  StorageConnectorOneDrive,
  StorageConnectors,
  ThemeName,
  UserSettings,
  StackAttribute' #-}

instance FromText StackAttribute where
    parser = (StackAttribute' . mk) <$> takeText

instance ToText StackAttribute where
    toText (StackAttribute' ci) = original ci

-- | Represents an enum of /known/ $StackAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackAttribute where
    toEnum i = case i of
        0 -> AccessEndpoints
        1 -> EmbedHostDomains
        2 -> FeedbackURL
        3 -> IAMRoleARN
        4 -> RedirectURL
        5 -> StorageConnectorGoogleDrive
        6 -> StorageConnectorHomefolders
        7 -> StorageConnectorOneDrive
        8 -> StorageConnectors
        9 -> ThemeName
        10 -> UserSettings
        _ -> (error . showText) $ "Unknown index for StackAttribute: " <> toText i
    fromEnum x = case x of
        AccessEndpoints -> 0
        EmbedHostDomains -> 1
        FeedbackURL -> 2
        IAMRoleARN -> 3
        RedirectURL -> 4
        StorageConnectorGoogleDrive -> 5
        StorageConnectorHomefolders -> 6
        StorageConnectorOneDrive -> 7
        StorageConnectors -> 8
        ThemeName -> 9
        UserSettings -> 10
        StackAttribute' name -> (error . showText) $ "Unknown StackAttribute: " <> original name

-- | Represents the bounds of /known/ $StackAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackAttribute where
    minBound = AccessEndpoints
    maxBound = UserSettings

instance Hashable     StackAttribute
instance NFData       StackAttribute
instance ToByteString StackAttribute
instance ToQuery      StackAttribute
instance ToHeader     StackAttribute

instance ToJSON StackAttribute where
    toJSON = toJSONText
