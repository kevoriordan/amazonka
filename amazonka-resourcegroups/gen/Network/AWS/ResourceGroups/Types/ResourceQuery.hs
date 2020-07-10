{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.ResourceQuery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.ResourceQuery where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroups.Types.QueryType

-- | The query that is used to define a resource group or a search for resources.
--
--
--
-- /See:/ 'resourceQuery' smart constructor.
data ResourceQuery = ResourceQuery'{_rqType ::
                                    !QueryType,
                                    _rqSearchQuery :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqType' - The type of the query. The valid values in this release are @TAG_FILTERS_1_0@ and @CLOUDFORMATION_STACK_1_0@ . /@TAG_FILTERS_1_0:@ / A JSON syntax that lets you specify a collection of simple tag filters for resource types and tags, as supported by the AWS Tagging API <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources> operation. If you specify more than one tag key, only resources that match all tag keys, and at least one value of each specified tag key, are returned in your query. If you specify more than one value for a tag key, a resource matches the filter if it has a tag key value that matches /any/ of the specified values. For example, consider the following sample query for resources that have two tags, @Stage@ and @Version@ , with two values each. (@[{"Key":"Stage","Values":["Test","Deploy"]},{"Key":"Version","Values":["1","2"]}]@ ) The results of this query might include the following.     * An EC2 instance that has the following two tags: @{"Key":"Stage","Value":"Deploy"}@ , and @{"Key":"Version","Value":"2"}@      * An S3 bucket that has the following two tags: {"Key":"Stage","Value":"Test"}, and {"Key":"Version","Value":"1"} The query would not return the following results, however. The following EC2 instance does not have all tag keys specified in the filter, so it is rejected. The RDS database has all of the tag keys, but no values that match at least one of the specified tag key values in the filter.     * An EC2 instance that has only the following tag: @{"Key":"Stage","Value":"Deploy"}@ .     * An RDS database that has the following two tags: @{"Key":"Stage","Value":"Archived"}@ , and @{"Key":"Version","Value":"4"}@  /@CLOUDFORMATION_STACK_1_0:@ / A JSON syntax that lets you specify a CloudFormation stack ARN.
--
-- * 'rqSearchQuery' - The query that defines a group or a search.
resourceQuery
    :: QueryType -- ^ 'rqType'
    -> Text -- ^ 'rqSearchQuery'
    -> ResourceQuery
resourceQuery pType_ pSearchQuery_
  = ResourceQuery'{_rqType = pType_,
                   _rqSearchQuery = pSearchQuery_}

-- | The type of the query. The valid values in this release are @TAG_FILTERS_1_0@ and @CLOUDFORMATION_STACK_1_0@ . /@TAG_FILTERS_1_0:@ / A JSON syntax that lets you specify a collection of simple tag filters for resource types and tags, as supported by the AWS Tagging API <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources> operation. If you specify more than one tag key, only resources that match all tag keys, and at least one value of each specified tag key, are returned in your query. If you specify more than one value for a tag key, a resource matches the filter if it has a tag key value that matches /any/ of the specified values. For example, consider the following sample query for resources that have two tags, @Stage@ and @Version@ , with two values each. (@[{"Key":"Stage","Values":["Test","Deploy"]},{"Key":"Version","Values":["1","2"]}]@ ) The results of this query might include the following.     * An EC2 instance that has the following two tags: @{"Key":"Stage","Value":"Deploy"}@ , and @{"Key":"Version","Value":"2"}@      * An S3 bucket that has the following two tags: {"Key":"Stage","Value":"Test"}, and {"Key":"Version","Value":"1"} The query would not return the following results, however. The following EC2 instance does not have all tag keys specified in the filter, so it is rejected. The RDS database has all of the tag keys, but no values that match at least one of the specified tag key values in the filter.     * An EC2 instance that has only the following tag: @{"Key":"Stage","Value":"Deploy"}@ .     * An RDS database that has the following two tags: @{"Key":"Stage","Value":"Archived"}@ , and @{"Key":"Version","Value":"4"}@  /@CLOUDFORMATION_STACK_1_0:@ / A JSON syntax that lets you specify a CloudFormation stack ARN.
rqType :: Lens' ResourceQuery QueryType
rqType = lens _rqType (\ s a -> s{_rqType = a})

-- | The query that defines a group or a search.
rqSearchQuery :: Lens' ResourceQuery Text
rqSearchQuery = lens _rqSearchQuery (\ s a -> s{_rqSearchQuery = a})

instance FromJSON ResourceQuery where
        parseJSON
          = withObject "ResourceQuery"
              (\ x ->
                 ResourceQuery' <$> (x .: "Type") <*> (x .: "Query"))

instance Hashable ResourceQuery where

instance NFData ResourceQuery where

instance ToJSON ResourceQuery where
        toJSON ResourceQuery'{..}
          = object
              (catMaybes
                 [Just ("Type" .= _rqType),
                  Just ("Query" .= _rqSearchQuery)])
