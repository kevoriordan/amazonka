{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListRoles
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the roles that have the specified path prefix. If there are none, the
-- action returns an empty list. For more information about roles, go to
-- Working with Roles. You can paginate the results using the MaxItems and
-- Marker parameters. The returned policy is URL-encoded according to RFC
-- 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=ListRoles &MaxItems=100 &PathPrefix=/application_abc/
-- &Version=2010-05-08 &AUTHPARAMS false /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:35Z AROACVSVTSZYEXAMPLEYK /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/SDBAccess
-- SDBAccess
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:45Z AROAC2ICXG32EXAMPLEWK
-- 20f7279f-99ee-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.ListRoles
    (
    -- * Request
      ListRoles
    -- ** Request constructor
    , listRoles
    -- ** Request lenses
    , lrPathPrefix
    , lrMarker
    , lrMaxItems

    -- * Response
    , ListRolesResponse
    -- ** Response constructor
    , listRolesResponse
    -- ** Response lenses
    , lrrRoles
    , lrrIsTruncated
    , lrrMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListRoles = ListRoles
    { _lrPathPrefix :: Maybe Text
    , _lrMarker :: Maybe Text
    , _lrMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListRoles' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PathPrefix ::@ @Maybe Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
listRoles :: ListRoles
listRoles = ListRoles
    { _lrPathPrefix = Nothing
    , _lrMarker = Nothing
    , _lrMaxItems = Nothing
    }

-- | The path prefix for filtering the results. For example:
-- /application_abc/component_xyz/, which would get all roles whose path
-- starts with /application_abc/component_xyz/. This parameter is optional. If
-- it is not included, it defaults to a slash (/), listing all roles.
lrPathPrefix :: Lens' ListRoles (Maybe Text)
lrPathPrefix = lens _lrPathPrefix (\s a -> s { _lrPathPrefix = a })

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lrMarker :: Lens' ListRoles (Maybe Text)
lrMarker = lens _lrMarker (\s a -> s { _lrMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of user names you want in the response. If there are additional user
-- names beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
lrMaxItems :: Lens' ListRoles (Maybe Integer)
lrMaxItems = lens _lrMaxItems (\s a -> s { _lrMaxItems = a })

instance ToQuery ListRoles where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListRoles action.
data ListRolesResponse = ListRolesResponse
    { _lrrRoles :: [Role]
    , _lrrIsTruncated :: !Bool
    , _lrrMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListRolesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Roles ::@ @[Role]@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Marker ::@ @Maybe Text@
--
listRolesResponse :: [Role] -- ^ 'lrrRoles'
                  -> Bool -- ^ 'lrrIsTruncated'
                  -> ListRolesResponse
listRolesResponse p1 p2 = ListRolesResponse
    { _lrrRoles = p1
    , _lrrIsTruncated = p2
    , _lrrMarker = Nothing
    }

-- | A list of roles.
lrrRoles :: Lens' ListRolesResponse [Role]
lrrRoles = lens _lrrRoles (\s a -> s { _lrrRoles = a })

-- | A flag that indicates whether there are more roles to list. If your results
-- were truncated, you can make a subsequent pagination request using the
-- Marker request parameter to retrieve more roles in the list.
lrrIsTruncated :: Lens' ListRolesResponse Bool
lrrIsTruncated = lens _lrrIsTruncated (\s a -> s { _lrrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lrrMarker :: Lens' ListRolesResponse (Maybe Text)
lrrMarker = lens _lrrMarker (\s a -> s { _lrrMarker = a })

instance FromXML ListRolesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListRoles where
    type Sv ListRoles = IAM
    type Rs ListRoles = ListRolesResponse

    request = post "ListRoles"
    response _ = xmlResponse

instance AWSPager ListRoles where
    next rq rs
        | not (rs ^. lrrIsTruncated) = Nothing
        | otherwise = Just $
            rq & lrMarker .~ rs ^. lrrMarker
