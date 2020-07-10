{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentType (
  DocumentType (
    ..
    , DTApplicationConfiguration
    , DTApplicationConfigurationSchema
    , DTAutomation
    , DTChangeCalendar
    , DTCommand
    , DTDeploymentStrategy
    , DTPackage
    , DTPolicy
    , DTSession
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DocumentType = DocumentType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern DTApplicationConfiguration :: DocumentType
pattern DTApplicationConfiguration = DocumentType' "ApplicationConfiguration"

pattern DTApplicationConfigurationSchema :: DocumentType
pattern DTApplicationConfigurationSchema = DocumentType' "ApplicationConfigurationSchema"

pattern DTAutomation :: DocumentType
pattern DTAutomation = DocumentType' "Automation"

pattern DTChangeCalendar :: DocumentType
pattern DTChangeCalendar = DocumentType' "ChangeCalendar"

pattern DTCommand :: DocumentType
pattern DTCommand = DocumentType' "Command"

pattern DTDeploymentStrategy :: DocumentType
pattern DTDeploymentStrategy = DocumentType' "DeploymentStrategy"

pattern DTPackage :: DocumentType
pattern DTPackage = DocumentType' "Package"

pattern DTPolicy :: DocumentType
pattern DTPolicy = DocumentType' "Policy"

pattern DTSession :: DocumentType
pattern DTSession = DocumentType' "Session"

{-# COMPLETE
  DTApplicationConfiguration,
  DTApplicationConfigurationSchema,
  DTAutomation,
  DTChangeCalendar,
  DTCommand,
  DTDeploymentStrategy,
  DTPackage,
  DTPolicy,
  DTSession,
  DocumentType' #-}

instance FromText DocumentType where
    parser = (DocumentType' . mk) <$> takeText

instance ToText DocumentType where
    toText (DocumentType' ci) = original ci

-- | Represents an enum of /known/ $DocumentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentType where
    toEnum i = case i of
        0 -> DTApplicationConfiguration
        1 -> DTApplicationConfigurationSchema
        2 -> DTAutomation
        3 -> DTChangeCalendar
        4 -> DTCommand
        5 -> DTDeploymentStrategy
        6 -> DTPackage
        7 -> DTPolicy
        8 -> DTSession
        _ -> (error . showText) $ "Unknown index for DocumentType: " <> toText i
    fromEnum x = case x of
        DTApplicationConfiguration -> 0
        DTApplicationConfigurationSchema -> 1
        DTAutomation -> 2
        DTChangeCalendar -> 3
        DTCommand -> 4
        DTDeploymentStrategy -> 5
        DTPackage -> 6
        DTPolicy -> 7
        DTSession -> 8
        DocumentType' name -> (error . showText) $ "Unknown DocumentType: " <> original name

-- | Represents the bounds of /known/ $DocumentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentType where
    minBound = DTApplicationConfiguration
    maxBound = DTSession

instance Hashable     DocumentType
instance NFData       DocumentType
instance ToByteString DocumentType
instance ToQuery      DocumentType
instance ToHeader     DocumentType

instance ToJSON DocumentType where
    toJSON = toJSONText

instance FromJSON DocumentType where
    parseJSON = parseJSONText "DocumentType"
