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
-- Module      : Network.AWS.IoT.DescribeSecurityProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender security profile.
--
--
module Network.AWS.IoT.DescribeSecurityProfile
    (
    -- * Creating a Request
      describeSecurityProfile
    , DescribeSecurityProfile
    -- * Request Lenses
    , dSecurityProfileName

    -- * Destructuring the Response
    , describeSecurityProfileResponse
    , DescribeSecurityProfileResponse
    -- * Response Lenses
    , dscrbscrtyprflrsAlertTargets
    , dscrbscrtyprflrsAdditionalMetricsToRetainV2
    , dscrbscrtyprflrsBehaviors
    , dscrbscrtyprflrsLastModifiedDate
    , dscrbscrtyprflrsVersion
    , dscrbscrtyprflrsSecurityProfileName
    , dscrbscrtyprflrsCreationDate
    , dscrbscrtyprflrsAdditionalMetricsToRetain
    , dscrbscrtyprflrsSecurityProfileARN
    , dscrbscrtyprflrsSecurityProfileDescription
    , dscrbscrtyprflrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSecurityProfile' smart constructor.
newtype DescribeSecurityProfile = DescribeSecurityProfile'{_dSecurityProfileName
                                                           :: Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DescribeSecurityProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSecurityProfileName' - The name of the security profile whose information you want to get.
describeSecurityProfile
    :: Text -- ^ 'dSecurityProfileName'
    -> DescribeSecurityProfile
describeSecurityProfile pSecurityProfileName_
  = DescribeSecurityProfile'{_dSecurityProfileName =
                               pSecurityProfileName_}

-- | The name of the security profile whose information you want to get.
dSecurityProfileName :: Lens' DescribeSecurityProfile Text
dSecurityProfileName = lens _dSecurityProfileName (\ s a -> s{_dSecurityProfileName = a})

instance AWSRequest DescribeSecurityProfile where
        type Rs DescribeSecurityProfile =
             DescribeSecurityProfileResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSecurityProfileResponse' <$>
                   (x .?> "alertTargets" .!@ mempty) <*>
                     (x .?> "additionalMetricsToRetainV2" .!@ mempty)
                     <*> (x .?> "behaviors" .!@ mempty)
                     <*> (x .?> "lastModifiedDate")
                     <*> (x .?> "version")
                     <*> (x .?> "securityProfileName")
                     <*> (x .?> "creationDate")
                     <*> (x .?> "additionalMetricsToRetain" .!@ mempty)
                     <*> (x .?> "securityProfileArn")
                     <*> (x .?> "securityProfileDescription")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSecurityProfile where

instance NFData DescribeSecurityProfile where

instance ToHeaders DescribeSecurityProfile where
        toHeaders = const mempty

instance ToPath DescribeSecurityProfile where
        toPath DescribeSecurityProfile'{..}
          = mconcat
              ["/security-profiles/", toBS _dSecurityProfileName]

instance ToQuery DescribeSecurityProfile where
        toQuery = const mempty

-- | /See:/ 'describeSecurityProfileResponse' smart constructor.
data DescribeSecurityProfileResponse = DescribeSecurityProfileResponse'{_dscrbscrtyprflrsAlertTargets
                                                                        ::
                                                                        !(Maybe
                                                                            (Map
                                                                               AlertTargetType
                                                                               AlertTarget)),
                                                                        _dscrbscrtyprflrsAdditionalMetricsToRetainV2
                                                                        ::
                                                                        !(Maybe
                                                                            [MetricToRetain]),
                                                                        _dscrbscrtyprflrsBehaviors
                                                                        ::
                                                                        !(Maybe
                                                                            [Behavior]),
                                                                        _dscrbscrtyprflrsLastModifiedDate
                                                                        ::
                                                                        !(Maybe
                                                                            POSIX),
                                                                        _dscrbscrtyprflrsVersion
                                                                        ::
                                                                        !(Maybe
                                                                            Integer),
                                                                        _dscrbscrtyprflrsSecurityProfileName
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _dscrbscrtyprflrsCreationDate
                                                                        ::
                                                                        !(Maybe
                                                                            POSIX),
                                                                        _dscrbscrtyprflrsAdditionalMetricsToRetain
                                                                        ::
                                                                        !(Maybe
                                                                            [Text]),
                                                                        _dscrbscrtyprflrsSecurityProfileARN
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _dscrbscrtyprflrsSecurityProfileDescription
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _dscrbscrtyprflrsResponseStatus
                                                                        :: !Int}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'DescribeSecurityProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbscrtyprflrsAlertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
--
-- * 'dscrbscrtyprflrsAdditionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- * 'dscrbscrtyprflrsBehaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- * 'dscrbscrtyprflrsLastModifiedDate' - The time the security profile was last modified.
--
-- * 'dscrbscrtyprflrsVersion' - The version of the security profile. A new version is generated whenever the security profile is updated.
--
-- * 'dscrbscrtyprflrsSecurityProfileName' - The name of the security profile.
--
-- * 'dscrbscrtyprflrsCreationDate' - The time the security profile was created.
--
-- * 'dscrbscrtyprflrsAdditionalMetricsToRetain' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here. __Note:__ This API field is deprecated. Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead.
--
-- * 'dscrbscrtyprflrsSecurityProfileARN' - The ARN of the security profile.
--
-- * 'dscrbscrtyprflrsSecurityProfileDescription' - A description of the security profile (associated with the security profile when it was created or updated).
--
-- * 'dscrbscrtyprflrsResponseStatus' - -- | The response status code.
describeSecurityProfileResponse
    :: Int -- ^ 'dscrbscrtyprflrsResponseStatus'
    -> DescribeSecurityProfileResponse
describeSecurityProfileResponse pResponseStatus_
  = DescribeSecurityProfileResponse'{_dscrbscrtyprflrsAlertTargets
                                       = Nothing,
                                     _dscrbscrtyprflrsAdditionalMetricsToRetainV2
                                       = Nothing,
                                     _dscrbscrtyprflrsBehaviors = Nothing,
                                     _dscrbscrtyprflrsLastModifiedDate =
                                       Nothing,
                                     _dscrbscrtyprflrsVersion = Nothing,
                                     _dscrbscrtyprflrsSecurityProfileName =
                                       Nothing,
                                     _dscrbscrtyprflrsCreationDate = Nothing,
                                     _dscrbscrtyprflrsAdditionalMetricsToRetain
                                       = Nothing,
                                     _dscrbscrtyprflrsSecurityProfileARN =
                                       Nothing,
                                     _dscrbscrtyprflrsSecurityProfileDescription
                                       = Nothing,
                                     _dscrbscrtyprflrsResponseStatus =
                                       pResponseStatus_}

-- | Where the alerts are sent. (Alerts are always sent to the console.)
dscrbscrtyprflrsAlertTargets :: Lens' DescribeSecurityProfileResponse (HashMap AlertTargetType AlertTarget)
dscrbscrtyprflrsAlertTargets = lens _dscrbscrtyprflrsAlertTargets (\ s a -> s{_dscrbscrtyprflrsAlertTargets = a}) . _Default . _Map

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
dscrbscrtyprflrsAdditionalMetricsToRetainV2 :: Lens' DescribeSecurityProfileResponse [MetricToRetain]
dscrbscrtyprflrsAdditionalMetricsToRetainV2 = lens _dscrbscrtyprflrsAdditionalMetricsToRetainV2 (\ s a -> s{_dscrbscrtyprflrsAdditionalMetricsToRetainV2 = a}) . _Default . _Coerce

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
dscrbscrtyprflrsBehaviors :: Lens' DescribeSecurityProfileResponse [Behavior]
dscrbscrtyprflrsBehaviors = lens _dscrbscrtyprflrsBehaviors (\ s a -> s{_dscrbscrtyprflrsBehaviors = a}) . _Default . _Coerce

-- | The time the security profile was last modified.
dscrbscrtyprflrsLastModifiedDate :: Lens' DescribeSecurityProfileResponse (Maybe UTCTime)
dscrbscrtyprflrsLastModifiedDate = lens _dscrbscrtyprflrsLastModifiedDate (\ s a -> s{_dscrbscrtyprflrsLastModifiedDate = a}) . mapping _Time

-- | The version of the security profile. A new version is generated whenever the security profile is updated.
dscrbscrtyprflrsVersion :: Lens' DescribeSecurityProfileResponse (Maybe Integer)
dscrbscrtyprflrsVersion = lens _dscrbscrtyprflrsVersion (\ s a -> s{_dscrbscrtyprflrsVersion = a})

-- | The name of the security profile.
dscrbscrtyprflrsSecurityProfileName :: Lens' DescribeSecurityProfileResponse (Maybe Text)
dscrbscrtyprflrsSecurityProfileName = lens _dscrbscrtyprflrsSecurityProfileName (\ s a -> s{_dscrbscrtyprflrsSecurityProfileName = a})

-- | The time the security profile was created.
dscrbscrtyprflrsCreationDate :: Lens' DescribeSecurityProfileResponse (Maybe UTCTime)
dscrbscrtyprflrsCreationDate = lens _dscrbscrtyprflrsCreationDate (\ s a -> s{_dscrbscrtyprflrsCreationDate = a}) . mapping _Time

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here. __Note:__ This API field is deprecated. Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead.
dscrbscrtyprflrsAdditionalMetricsToRetain :: Lens' DescribeSecurityProfileResponse [Text]
dscrbscrtyprflrsAdditionalMetricsToRetain = lens _dscrbscrtyprflrsAdditionalMetricsToRetain (\ s a -> s{_dscrbscrtyprflrsAdditionalMetricsToRetain = a}) . _Default . _Coerce

-- | The ARN of the security profile.
dscrbscrtyprflrsSecurityProfileARN :: Lens' DescribeSecurityProfileResponse (Maybe Text)
dscrbscrtyprflrsSecurityProfileARN = lens _dscrbscrtyprflrsSecurityProfileARN (\ s a -> s{_dscrbscrtyprflrsSecurityProfileARN = a})

-- | A description of the security profile (associated with the security profile when it was created or updated).
dscrbscrtyprflrsSecurityProfileDescription :: Lens' DescribeSecurityProfileResponse (Maybe Text)
dscrbscrtyprflrsSecurityProfileDescription = lens _dscrbscrtyprflrsSecurityProfileDescription (\ s a -> s{_dscrbscrtyprflrsSecurityProfileDescription = a})

-- | -- | The response status code.
dscrbscrtyprflrsResponseStatus :: Lens' DescribeSecurityProfileResponse Int
dscrbscrtyprflrsResponseStatus = lens _dscrbscrtyprflrsResponseStatus (\ s a -> s{_dscrbscrtyprflrsResponseStatus = a})

instance NFData DescribeSecurityProfileResponse where
