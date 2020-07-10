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
-- Module      : Network.AWS.SageMaker.CreateDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Domain for Amazon SageMaker Amazon SageMaker Studio (Studio), which can be accessed by end-users in a web browser. A Domain has an associated directory, list of authorized users, and a variety of security, application, policies, and Amazon Virtual Private Cloud configurations. An AWS account is limited to one Domain, per region. Users within a domain can share notebook files and other artifacts with each other. When a Domain is created, an Amazon Elastic File System (EFS) is also created for use by all of the users within the Domain. Each user receives a private home directory within the EFS for notebooks, Git repositories, and data files. 
--
--
module Network.AWS.SageMaker.CreateDomain
    (
    -- * Creating a Request
      createDomain
    , CreateDomain
    -- * Request Lenses
    , cdHomeEfsFileSystemKMSKeyId
    , cdTags
    , cdDomainName
    , cdAuthMode
    , cdDefaultUserSettings
    , cdSubnetIds
    , cdVPCId

    -- * Destructuring the Response
    , createDomainResponse
    , CreateDomainResponse
    -- * Response Lenses
    , cdrsDomainARN
    , cdrsURL
    , cdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createDomain' smart constructor.
data CreateDomain = CreateDomain'{_cdHomeEfsFileSystemKMSKeyId
                                  :: !(Maybe Text),
                                  _cdTags :: !(Maybe [Tag]),
                                  _cdDomainName :: !Text,
                                  _cdAuthMode :: !AuthMode,
                                  _cdDefaultUserSettings :: !UserSettings,
                                  _cdSubnetIds :: !(List1 Text),
                                  _cdVPCId :: !Text}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdHomeEfsFileSystemKMSKeyId' - The AWS Key Management Service encryption key ID.
--
-- * 'cdTags' - Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- * 'cdDomainName' - A name for the domain.
--
-- * 'cdAuthMode' - The mode of authentication that member use to access the domain.
--
-- * 'cdDefaultUserSettings' - The default user settings.
--
-- * 'cdSubnetIds' - Security setting to limit to a set of subnets.
--
-- * 'cdVPCId' - Security setting to limit the domain's communication to a Amazon Virtual Private Cloud.
createDomain
    :: Text -- ^ 'cdDomainName'
    -> AuthMode -- ^ 'cdAuthMode'
    -> UserSettings -- ^ 'cdDefaultUserSettings'
    -> NonEmpty Text -- ^ 'cdSubnetIds'
    -> Text -- ^ 'cdVPCId'
    -> CreateDomain
createDomain pDomainName_ pAuthMode_
  pDefaultUserSettings_ pSubnetIds_ pVPCId_
  = CreateDomain'{_cdHomeEfsFileSystemKMSKeyId =
                    Nothing,
                  _cdTags = Nothing, _cdDomainName = pDomainName_,
                  _cdAuthMode = pAuthMode_,
                  _cdDefaultUserSettings = pDefaultUserSettings_,
                  _cdSubnetIds = _List1 # pSubnetIds_,
                  _cdVPCId = pVPCId_}

-- | The AWS Key Management Service encryption key ID.
cdHomeEfsFileSystemKMSKeyId :: Lens' CreateDomain (Maybe Text)
cdHomeEfsFileSystemKMSKeyId = lens _cdHomeEfsFileSystemKMSKeyId (\ s a -> s{_cdHomeEfsFileSystemKMSKeyId = a})

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
cdTags :: Lens' CreateDomain [Tag]
cdTags = lens _cdTags (\ s a -> s{_cdTags = a}) . _Default . _Coerce

-- | A name for the domain.
cdDomainName :: Lens' CreateDomain Text
cdDomainName = lens _cdDomainName (\ s a -> s{_cdDomainName = a})

-- | The mode of authentication that member use to access the domain.
cdAuthMode :: Lens' CreateDomain AuthMode
cdAuthMode = lens _cdAuthMode (\ s a -> s{_cdAuthMode = a})

-- | The default user settings.
cdDefaultUserSettings :: Lens' CreateDomain UserSettings
cdDefaultUserSettings = lens _cdDefaultUserSettings (\ s a -> s{_cdDefaultUserSettings = a})

-- | Security setting to limit to a set of subnets.
cdSubnetIds :: Lens' CreateDomain (NonEmpty Text)
cdSubnetIds = lens _cdSubnetIds (\ s a -> s{_cdSubnetIds = a}) . _List1

-- | Security setting to limit the domain's communication to a Amazon Virtual Private Cloud.
cdVPCId :: Lens' CreateDomain Text
cdVPCId = lens _cdVPCId (\ s a -> s{_cdVPCId = a})

instance AWSRequest CreateDomain where
        type Rs CreateDomain = CreateDomainResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateDomainResponse' <$>
                   (x .?> "DomainArn") <*> (x .?> "Url") <*>
                     (pure (fromEnum s)))

instance Hashable CreateDomain where

instance NFData CreateDomain where

instance ToHeaders CreateDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateDomain" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDomain where
        toJSON CreateDomain'{..}
          = object
              (catMaybes
                 [("HomeEfsFileSystemKmsKeyId" .=) <$>
                    _cdHomeEfsFileSystemKMSKeyId,
                  ("Tags" .=) <$> _cdTags,
                  Just ("DomainName" .= _cdDomainName),
                  Just ("AuthMode" .= _cdAuthMode),
                  Just
                    ("DefaultUserSettings" .= _cdDefaultUserSettings),
                  Just ("SubnetIds" .= _cdSubnetIds),
                  Just ("VpcId" .= _cdVPCId)])

instance ToPath CreateDomain where
        toPath = const "/"

instance ToQuery CreateDomain where
        toQuery = const mempty

-- | /See:/ 'createDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'{_cdrsDomainARN
                                                  :: !(Maybe Text),
                                                  _cdrsURL :: !(Maybe Text),
                                                  _cdrsResponseStatus :: !Int}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDomainARN' - The Amazon Resource Name (ARN) of the created domain.
--
-- * 'cdrsURL' - The URL to the created domain.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDomainResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDomainResponse
createDomainResponse pResponseStatus_
  = CreateDomainResponse'{_cdrsDomainARN = Nothing,
                          _cdrsURL = Nothing,
                          _cdrsResponseStatus = pResponseStatus_}

-- | The Amazon Resource Name (ARN) of the created domain.
cdrsDomainARN :: Lens' CreateDomainResponse (Maybe Text)
cdrsDomainARN = lens _cdrsDomainARN (\ s a -> s{_cdrsDomainARN = a})

-- | The URL to the created domain.
cdrsURL :: Lens' CreateDomainResponse (Maybe Text)
cdrsURL = lens _cdrsURL (\ s a -> s{_cdrsURL = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDomainResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDomainResponse where
