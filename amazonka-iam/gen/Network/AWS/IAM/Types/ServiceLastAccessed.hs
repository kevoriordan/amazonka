{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServiceLastAccessed
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.ServiceLastAccessed where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the most recent attempt to access the service.
--
--
-- This data type is used as a response element in the 'GetServiceLastAccessedDetails' operation.
--
--
-- /See:/ 'serviceLastAccessed' smart constructor.
data ServiceLastAccessed = ServiceLastAccessed'{_slaLastAuthenticated
                                                :: !(Maybe ISO8601),
                                                _slaLastAuthenticatedEntity ::
                                                !(Maybe Text),
                                                _slaTotalAuthenticatedEntities
                                                :: !(Maybe Int),
                                                _slaServiceName :: !Text,
                                                _slaServiceNamespace :: !Text}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceLastAccessed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slaLastAuthenticated' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the service. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- * 'slaLastAuthenticatedEntity' - The ARN of the authenticated entity (user or role) that last attempted to access the service. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- * 'slaTotalAuthenticatedEntities' - The total number of authenticated principals (root user, IAM users, or IAM roles) that have attempted to access the service. This field is null if no principals attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- * 'slaServiceName' - The name of the service in which access was attempted.
--
-- * 'slaServiceNamespace' - The namespace of the service in which access was attempted. To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
serviceLastAccessed
    :: Text -- ^ 'slaServiceName'
    -> Text -- ^ 'slaServiceNamespace'
    -> ServiceLastAccessed
serviceLastAccessed pServiceName_ pServiceNamespace_
  = ServiceLastAccessed'{_slaLastAuthenticated =
                           Nothing,
                         _slaLastAuthenticatedEntity = Nothing,
                         _slaTotalAuthenticatedEntities = Nothing,
                         _slaServiceName = pServiceName_,
                         _slaServiceNamespace = pServiceNamespace_}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the service. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
slaLastAuthenticated :: Lens' ServiceLastAccessed (Maybe UTCTime)
slaLastAuthenticated = lens _slaLastAuthenticated (\ s a -> s{_slaLastAuthenticated = a}) . mapping _Time

-- | The ARN of the authenticated entity (user or role) that last attempted to access the service. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
slaLastAuthenticatedEntity :: Lens' ServiceLastAccessed (Maybe Text)
slaLastAuthenticatedEntity = lens _slaLastAuthenticatedEntity (\ s a -> s{_slaLastAuthenticatedEntity = a})

-- | The total number of authenticated principals (root user, IAM users, or IAM roles) that have attempted to access the service. This field is null if no principals attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
slaTotalAuthenticatedEntities :: Lens' ServiceLastAccessed (Maybe Int)
slaTotalAuthenticatedEntities = lens _slaTotalAuthenticatedEntities (\ s a -> s{_slaTotalAuthenticatedEntities = a})

-- | The name of the service in which access was attempted.
slaServiceName :: Lens' ServiceLastAccessed Text
slaServiceName = lens _slaServiceName (\ s a -> s{_slaServiceName = a})

-- | The namespace of the service in which access was attempted. To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
slaServiceNamespace :: Lens' ServiceLastAccessed Text
slaServiceNamespace = lens _slaServiceNamespace (\ s a -> s{_slaServiceNamespace = a})

instance FromXML ServiceLastAccessed where
        parseXML x
          = ServiceLastAccessed' <$>
              (x .@? "LastAuthenticated") <*>
                (x .@? "LastAuthenticatedEntity")
                <*> (x .@? "TotalAuthenticatedEntities")
                <*> (x .@ "ServiceName")
                <*> (x .@ "ServiceNamespace")

instance Hashable ServiceLastAccessed where

instance NFData ServiceLastAccessed where
