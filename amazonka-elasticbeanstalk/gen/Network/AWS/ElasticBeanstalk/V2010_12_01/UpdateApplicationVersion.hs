{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified application version to have the specified properties.
-- If a property (for example, description) is not provided, the value remains
-- unchanged. To clear properties, specify an empty string.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=New%20Version &Description=New%20Release%20Description
-- &Operation=UpdateApplicationVersion &AuthParams awsemr sample.war New
-- Version New Release Description SampleApp 2010-11-17T19:26:20.699Z
-- 2010-11-17T20:48:16.632Z 00b10aa1-f28c-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion
    (
    -- * Request
      UpdateApplicationVersion
    -- ** Request constructor
    , mkUpdateApplicationVersion
    -- ** Request lenses
    , uavApplicationName
    , uavVersionLabel
    , uavDescription

    -- * Response
    , UpdateApplicationVersionResponse
    -- ** Response constructor
    , mkUpdateApplicationVersionResponse
    -- ** Response lenses
    , uavrApplicationVersion
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | 
data UpdateApplicationVersion = UpdateApplicationVersion
    { _uavApplicationName :: Text
    , _uavVersionLabel :: Text
    , _uavDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateApplicationVersion' request.
mkUpdateApplicationVersion :: Text -- ^ 'uavApplicationName'
                           -> Text -- ^ 'uavVersionLabel'
                           -> UpdateApplicationVersion
mkUpdateApplicationVersion p1 p2 = UpdateApplicationVersion
    { _uavApplicationName = p1
    , _uavVersionLabel = p2
    , _uavDescription = Nothing
    }

-- | The name of the application associated with this version. If no application
-- is found with this name, UpdateApplication returns an InvalidParameterValue
-- error.
uavApplicationName :: Lens' UpdateApplicationVersion Text
uavApplicationName =
    lens _uavApplicationName (\s a -> s { _uavApplicationName = a })

-- | The name of the version to update. If no application version is found with
-- this label, UpdateApplication returns an InvalidParameterValue error.
uavVersionLabel :: Lens' UpdateApplicationVersion Text
uavVersionLabel = lens _uavVersionLabel (\s a -> s { _uavVersionLabel = a })

-- | A new description for this release.
uavDescription :: Lens' UpdateApplicationVersion (Maybe Text)
uavDescription = lens _uavDescription (\s a -> s { _uavDescription = a })

instance ToQuery UpdateApplicationVersion where
    toQuery = genericQuery def

-- | Result message wrapping a single description of an application version.
newtype UpdateApplicationVersionResponse = UpdateApplicationVersionResponse
    { _uavrApplicationVersion :: Maybe ApplicationVersionDescription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateApplicationVersionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateApplicationVersionResponse :: UpdateApplicationVersionResponse
mkUpdateApplicationVersionResponse = UpdateApplicationVersionResponse
    { _uavrApplicationVersion = Nothing
    }

-- | The ApplicationVersionDescription of the application version.
uavrApplicationVersion :: Lens' UpdateApplicationVersionResponse (Maybe ApplicationVersionDescription)
uavrApplicationVersion =
    lens _uavrApplicationVersion (\s a -> s { _uavrApplicationVersion = a })

instance FromXML UpdateApplicationVersionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateApplicationVersion where
    type Sv UpdateApplicationVersion = ElasticBeanstalk
    type Rs UpdateApplicationVersion = UpdateApplicationVersionResponse

    request = post "UpdateApplicationVersion"
    response _ = xmlResponse
