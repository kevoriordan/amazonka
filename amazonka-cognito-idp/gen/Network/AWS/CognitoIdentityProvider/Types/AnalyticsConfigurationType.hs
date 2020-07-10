{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon Pinpoint analytics configuration for collecting metrics for a user pool.
--
--
--
-- /See:/ 'analyticsConfigurationType' smart constructor.
data AnalyticsConfigurationType = AnalyticsConfigurationType'{_actUserDataShared
                                                              :: !(Maybe Bool),
                                                              _actApplicationId
                                                              :: !Text,
                                                              _actRoleARN ::
                                                              !Text,
                                                              _actExternalId ::
                                                              !Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'AnalyticsConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'actUserDataShared' - If @UserDataShared@ is @true@ , Amazon Cognito will include user data in the events it publishes to Amazon Pinpoint analytics.
--
-- * 'actApplicationId' - The application ID for an Amazon Pinpoint application.
--
-- * 'actRoleARN' - The ARN of an IAM role that authorizes Amazon Cognito to publish events to Amazon Pinpoint analytics.
--
-- * 'actExternalId' - The external ID.
analyticsConfigurationType
    :: Text -- ^ 'actApplicationId'
    -> Text -- ^ 'actRoleARN'
    -> Text -- ^ 'actExternalId'
    -> AnalyticsConfigurationType
analyticsConfigurationType pApplicationId_ pRoleARN_
  pExternalId_
  = AnalyticsConfigurationType'{_actUserDataShared =
                                  Nothing,
                                _actApplicationId = pApplicationId_,
                                _actRoleARN = pRoleARN_,
                                _actExternalId = pExternalId_}

-- | If @UserDataShared@ is @true@ , Amazon Cognito will include user data in the events it publishes to Amazon Pinpoint analytics.
actUserDataShared :: Lens' AnalyticsConfigurationType (Maybe Bool)
actUserDataShared = lens _actUserDataShared (\ s a -> s{_actUserDataShared = a})

-- | The application ID for an Amazon Pinpoint application.
actApplicationId :: Lens' AnalyticsConfigurationType Text
actApplicationId = lens _actApplicationId (\ s a -> s{_actApplicationId = a})

-- | The ARN of an IAM role that authorizes Amazon Cognito to publish events to Amazon Pinpoint analytics.
actRoleARN :: Lens' AnalyticsConfigurationType Text
actRoleARN = lens _actRoleARN (\ s a -> s{_actRoleARN = a})

-- | The external ID.
actExternalId :: Lens' AnalyticsConfigurationType Text
actExternalId = lens _actExternalId (\ s a -> s{_actExternalId = a})

instance FromJSON AnalyticsConfigurationType where
        parseJSON
          = withObject "AnalyticsConfigurationType"
              (\ x ->
                 AnalyticsConfigurationType' <$>
                   (x .:? "UserDataShared") <*> (x .: "ApplicationId")
                     <*> (x .: "RoleArn")
                     <*> (x .: "ExternalId"))

instance Hashable AnalyticsConfigurationType where

instance NFData AnalyticsConfigurationType where

instance ToJSON AnalyticsConfigurationType where
        toJSON AnalyticsConfigurationType'{..}
          = object
              (catMaybes
                 [("UserDataShared" .=) <$> _actUserDataShared,
                  Just ("ApplicationId" .= _actApplicationId),
                  Just ("RoleArn" .= _actRoleARN),
                  Just ("ExternalId" .= _actExternalId)])
