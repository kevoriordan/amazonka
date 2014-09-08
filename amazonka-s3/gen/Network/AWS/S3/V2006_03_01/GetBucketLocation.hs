{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the region the bucket resides in.
module Network.AWS.S3.V2006_03_01.GetBucketLocation
    (
    -- * Request
      GetBucketLocation
    -- ** Request constructor
    , mkGetBucketLocation
    -- ** Request lenses
    , gbl1Bucket

    -- * Response
    , GetBucketLocationResponse
    -- ** Response constructor
    , mkGetBucketLocationResponse
    -- ** Response lenses
    , gblrrLocationConstraint
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketLocation = GetBucketLocation
    { _gbl1Bucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketLocation' request.
mkGetBucketLocation :: BucketName -- ^ 'gbl1Bucket'
                    -> GetBucketLocation
mkGetBucketLocation p1 = GetBucketLocation
    { _gbl1Bucket = p1
    }

gbl1Bucket :: Lens' GetBucketLocation BucketName
gbl1Bucket = lens _gbl1Bucket (\s a -> s { _gbl1Bucket = a })

instance ToPath GetBucketLocation where
    toPath GetBucketLocation{..} = mconcat
        [ "/"
        , toBS _gbl1Bucket
        ]

instance ToQuery GetBucketLocation where
    toQuery GetBucketLocation{..} = mconcat
        [ "location"
        ]

instance ToHeaders GetBucketLocation

instance ToBody GetBucketLocation

newtype GetBucketLocationResponse = GetBucketLocationResponse
    { _gblrrLocationConstraint :: Maybe Region
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketLocationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkGetBucketLocationResponse :: GetBucketLocationResponse
mkGetBucketLocationResponse = GetBucketLocationResponse
    { _gblrrLocationConstraint = Nothing
    }

gblrrLocationConstraint :: Lens' GetBucketLocationResponse (Maybe Region)
gblrrLocationConstraint =
    lens _gblrrLocationConstraint
         (\s a -> s { _gblrrLocationConstraint = a })

instance FromXML GetBucketLocationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketLocation where
    type Sv GetBucketLocation = S3
    type Rs GetBucketLocation = GetBucketLocationResponse

    request = get
    response _ = xmlResponse
