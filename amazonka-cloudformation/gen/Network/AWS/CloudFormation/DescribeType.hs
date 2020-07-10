{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about a type that has been registered.
--
--
-- If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
--
module Network.AWS.CloudFormation.DescribeType
    (
    -- * Creating a Request
      describeType
    , DescribeType
    -- * Request Lenses
    , dtVersionId
    , dtTypeName
    , dtARN
    , dtType

    -- * Destructuring the Response
    , describeTypeResponse
    , DescribeTypeResponse
    -- * Response Lenses
    , dscrbtyprsLastUpdated
    , dscrbtyprsTypeName
    , dscrbtyprsARN
    , dscrbtyprsExecutionRoleARN
    , dscrbtyprsVisibility
    , dscrbtyprsSchema
    , dscrbtyprsDefaultVersionId
    , dscrbtyprsDeprecatedStatus
    , dscrbtyprsTimeCreated
    , dscrbtyprsType
    , dscrbtyprsDescription
    , dscrbtyprsSourceURL
    , dscrbtyprsDocumentationURL
    , dscrbtyprsProvisioningType
    , dscrbtyprsLoggingConfig
    , dscrbtyprsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeType' smart constructor.
data DescribeType = DescribeType'{_dtVersionId ::
                                  !(Maybe Text),
                                  _dtTypeName :: !(Maybe Text),
                                  _dtARN :: !(Maybe Text),
                                  _dtType :: !(Maybe RegistryType)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtVersionId' - The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered. If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
--
-- * 'dtTypeName' - The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'dtARN' - The Amazon Resource Name (ARN) of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'dtType' - The kind of type.  Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
describeType
    :: DescribeType
describeType
  = DescribeType'{_dtVersionId = Nothing,
                  _dtTypeName = Nothing, _dtARN = Nothing,
                  _dtType = Nothing}

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered. If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
dtVersionId :: Lens' DescribeType (Maybe Text)
dtVersionId = lens _dtVersionId (\ s a -> s{_dtVersionId = a})

-- | The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
dtTypeName :: Lens' DescribeType (Maybe Text)
dtTypeName = lens _dtTypeName (\ s a -> s{_dtTypeName = a})

-- | The Amazon Resource Name (ARN) of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
dtARN :: Lens' DescribeType (Maybe Text)
dtARN = lens _dtARN (\ s a -> s{_dtARN = a})

-- | The kind of type.  Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
dtType :: Lens' DescribeType (Maybe RegistryType)
dtType = lens _dtType (\ s a -> s{_dtType = a})

instance AWSRequest DescribeType where
        type Rs DescribeType = DescribeTypeResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DescribeTypeResult"
              (\ s h x ->
                 DescribeTypeResponse' <$>
                   (x .@? "LastUpdated") <*> (x .@? "TypeName") <*>
                     (x .@? "Arn")
                     <*> (x .@? "ExecutionRoleArn")
                     <*> (x .@? "Visibility")
                     <*> (x .@? "Schema")
                     <*> (x .@? "DefaultVersionId")
                     <*> (x .@? "DeprecatedStatus")
                     <*> (x .@? "TimeCreated")
                     <*> (x .@? "Type")
                     <*> (x .@? "Description")
                     <*> (x .@? "SourceUrl")
                     <*> (x .@? "DocumentationUrl")
                     <*> (x .@? "ProvisioningType")
                     <*> (x .@? "LoggingConfig")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeType where

instance NFData DescribeType where

instance ToHeaders DescribeType where
        toHeaders = const mempty

instance ToPath DescribeType where
        toPath = const "/"

instance ToQuery DescribeType where
        toQuery DescribeType'{..}
          = mconcat
              ["Action" =: ("DescribeType" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "VersionId" =: _dtVersionId,
               "TypeName" =: _dtTypeName, "Arn" =: _dtARN,
               "Type" =: _dtType]

-- | /See:/ 'describeTypeResponse' smart constructor.
data DescribeTypeResponse = DescribeTypeResponse'{_dscrbtyprsLastUpdated
                                                  :: !(Maybe ISO8601),
                                                  _dscrbtyprsTypeName ::
                                                  !(Maybe Text),
                                                  _dscrbtyprsARN ::
                                                  !(Maybe Text),
                                                  _dscrbtyprsExecutionRoleARN ::
                                                  !(Maybe Text),
                                                  _dscrbtyprsVisibility ::
                                                  !(Maybe Visibility),
                                                  _dscrbtyprsSchema ::
                                                  !(Maybe Text),
                                                  _dscrbtyprsDefaultVersionId ::
                                                  !(Maybe Text),
                                                  _dscrbtyprsDeprecatedStatus ::
                                                  !(Maybe DeprecatedStatus),
                                                  _dscrbtyprsTimeCreated ::
                                                  !(Maybe ISO8601),
                                                  _dscrbtyprsType ::
                                                  !(Maybe RegistryType),
                                                  _dscrbtyprsDescription ::
                                                  !(Maybe Text),
                                                  _dscrbtyprsSourceURL ::
                                                  !(Maybe Text),
                                                  _dscrbtyprsDocumentationURL ::
                                                  !(Maybe Text),
                                                  _dscrbtyprsProvisioningType ::
                                                  !(Maybe ProvisioningType),
                                                  _dscrbtyprsLoggingConfig ::
                                                  !(Maybe LoggingConfig),
                                                  _dscrbtyprsResponseStatus ::
                                                  !Int}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbtyprsLastUpdated' - When the specified type version was registered.
--
-- * 'dscrbtyprsTypeName' - The name of the registered type.
--
-- * 'dscrbtyprsARN' - The Amazon Resource Name (ARN) of the type.
--
-- * 'dscrbtyprsExecutionRoleARN' - The Amazon Resource Name (ARN) of the IAM execution role used to register the type. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. CloudFormation then assumes that execution role to provide your resource type with the appropriate credentials.
--
-- * 'dscrbtyprsVisibility' - The scope at which the type is visible and usable in CloudFormation operations. Valid values include:     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you register as @PRIVATE@ .     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
--
-- * 'dscrbtyprsSchema' - The schema that defines the type. For more information on type schemas, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema> in the /CloudFormation CLI User Guide/ .
--
-- * 'dscrbtyprsDefaultVersionId' - The ID of the default version of the type. The default version is used when the type version is not specified. To set the default version of a type, use @'SetTypeDefaultVersion' @ . 
--
-- * 'dscrbtyprsDeprecatedStatus' - The deprecation status of the type. Valid values include:     * @LIVE@ : The type is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations. 
--
-- * 'dscrbtyprsTimeCreated' - When the specified type version was registered.
--
-- * 'dscrbtyprsType' - The kind of type.  Currently the only valid value is @RESOURCE@ .
--
-- * 'dscrbtyprsDescription' - The description of the registered type.
--
-- * 'dscrbtyprsSourceURL' - The URL of the source code for the type.
--
-- * 'dscrbtyprsDocumentationURL' - The URL of a page providing detailed documentation for this type.
--
-- * 'dscrbtyprsProvisioningType' - The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted. Valid values include:     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.     * @NON_PROVISIONABLE@ : The type does not include all of the following handlers, and therefore cannot actually be provisioned.     * create     * read     * delete
--
-- * 'dscrbtyprsLoggingConfig' - Contains logging configuration information for a type.
--
-- * 'dscrbtyprsResponseStatus' - -- | The response status code.
describeTypeResponse
    :: Int -- ^ 'dscrbtyprsResponseStatus'
    -> DescribeTypeResponse
describeTypeResponse pResponseStatus_
  = DescribeTypeResponse'{_dscrbtyprsLastUpdated =
                            Nothing,
                          _dscrbtyprsTypeName = Nothing,
                          _dscrbtyprsARN = Nothing,
                          _dscrbtyprsExecutionRoleARN = Nothing,
                          _dscrbtyprsVisibility = Nothing,
                          _dscrbtyprsSchema = Nothing,
                          _dscrbtyprsDefaultVersionId = Nothing,
                          _dscrbtyprsDeprecatedStatus = Nothing,
                          _dscrbtyprsTimeCreated = Nothing,
                          _dscrbtyprsType = Nothing,
                          _dscrbtyprsDescription = Nothing,
                          _dscrbtyprsSourceURL = Nothing,
                          _dscrbtyprsDocumentationURL = Nothing,
                          _dscrbtyprsProvisioningType = Nothing,
                          _dscrbtyprsLoggingConfig = Nothing,
                          _dscrbtyprsResponseStatus = pResponseStatus_}

-- | When the specified type version was registered.
dscrbtyprsLastUpdated :: Lens' DescribeTypeResponse (Maybe UTCTime)
dscrbtyprsLastUpdated = lens _dscrbtyprsLastUpdated (\ s a -> s{_dscrbtyprsLastUpdated = a}) . mapping _Time

-- | The name of the registered type.
dscrbtyprsTypeName :: Lens' DescribeTypeResponse (Maybe Text)
dscrbtyprsTypeName = lens _dscrbtyprsTypeName (\ s a -> s{_dscrbtyprsTypeName = a})

-- | The Amazon Resource Name (ARN) of the type.
dscrbtyprsARN :: Lens' DescribeTypeResponse (Maybe Text)
dscrbtyprsARN = lens _dscrbtyprsARN (\ s a -> s{_dscrbtyprsARN = a})

-- | The Amazon Resource Name (ARN) of the IAM execution role used to register the type. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. CloudFormation then assumes that execution role to provide your resource type with the appropriate credentials.
dscrbtyprsExecutionRoleARN :: Lens' DescribeTypeResponse (Maybe Text)
dscrbtyprsExecutionRoleARN = lens _dscrbtyprsExecutionRoleARN (\ s a -> s{_dscrbtyprsExecutionRoleARN = a})

-- | The scope at which the type is visible and usable in CloudFormation operations. Valid values include:     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you register as @PRIVATE@ .     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
dscrbtyprsVisibility :: Lens' DescribeTypeResponse (Maybe Visibility)
dscrbtyprsVisibility = lens _dscrbtyprsVisibility (\ s a -> s{_dscrbtyprsVisibility = a})

-- | The schema that defines the type. For more information on type schemas, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema> in the /CloudFormation CLI User Guide/ .
dscrbtyprsSchema :: Lens' DescribeTypeResponse (Maybe Text)
dscrbtyprsSchema = lens _dscrbtyprsSchema (\ s a -> s{_dscrbtyprsSchema = a})

-- | The ID of the default version of the type. The default version is used when the type version is not specified. To set the default version of a type, use @'SetTypeDefaultVersion' @ . 
dscrbtyprsDefaultVersionId :: Lens' DescribeTypeResponse (Maybe Text)
dscrbtyprsDefaultVersionId = lens _dscrbtyprsDefaultVersionId (\ s a -> s{_dscrbtyprsDefaultVersionId = a})

-- | The deprecation status of the type. Valid values include:     * @LIVE@ : The type is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations. 
dscrbtyprsDeprecatedStatus :: Lens' DescribeTypeResponse (Maybe DeprecatedStatus)
dscrbtyprsDeprecatedStatus = lens _dscrbtyprsDeprecatedStatus (\ s a -> s{_dscrbtyprsDeprecatedStatus = a})

-- | When the specified type version was registered.
dscrbtyprsTimeCreated :: Lens' DescribeTypeResponse (Maybe UTCTime)
dscrbtyprsTimeCreated = lens _dscrbtyprsTimeCreated (\ s a -> s{_dscrbtyprsTimeCreated = a}) . mapping _Time

-- | The kind of type.  Currently the only valid value is @RESOURCE@ .
dscrbtyprsType :: Lens' DescribeTypeResponse (Maybe RegistryType)
dscrbtyprsType = lens _dscrbtyprsType (\ s a -> s{_dscrbtyprsType = a})

-- | The description of the registered type.
dscrbtyprsDescription :: Lens' DescribeTypeResponse (Maybe Text)
dscrbtyprsDescription = lens _dscrbtyprsDescription (\ s a -> s{_dscrbtyprsDescription = a})

-- | The URL of the source code for the type.
dscrbtyprsSourceURL :: Lens' DescribeTypeResponse (Maybe Text)
dscrbtyprsSourceURL = lens _dscrbtyprsSourceURL (\ s a -> s{_dscrbtyprsSourceURL = a})

-- | The URL of a page providing detailed documentation for this type.
dscrbtyprsDocumentationURL :: Lens' DescribeTypeResponse (Maybe Text)
dscrbtyprsDocumentationURL = lens _dscrbtyprsDocumentationURL (\ s a -> s{_dscrbtyprsDocumentationURL = a})

-- | The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted. Valid values include:     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.     * @NON_PROVISIONABLE@ : The type does not include all of the following handlers, and therefore cannot actually be provisioned.     * create     * read     * delete
dscrbtyprsProvisioningType :: Lens' DescribeTypeResponse (Maybe ProvisioningType)
dscrbtyprsProvisioningType = lens _dscrbtyprsProvisioningType (\ s a -> s{_dscrbtyprsProvisioningType = a})

-- | Contains logging configuration information for a type.
dscrbtyprsLoggingConfig :: Lens' DescribeTypeResponse (Maybe LoggingConfig)
dscrbtyprsLoggingConfig = lens _dscrbtyprsLoggingConfig (\ s a -> s{_dscrbtyprsLoggingConfig = a})

-- | -- | The response status code.
dscrbtyprsResponseStatus :: Lens' DescribeTypeResponse Int
dscrbtyprsResponseStatus = lens _dscrbtyprsResponseStatus (\ s a -> s{_dscrbtyprsResponseStatus = a})

instance NFData DescribeTypeResponse where
