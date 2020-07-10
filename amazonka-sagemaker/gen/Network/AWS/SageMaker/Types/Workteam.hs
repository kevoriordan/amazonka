{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Workteam
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Workteam where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.MemberDefinition
import Network.AWS.SageMaker.Types.NotificationConfiguration

-- | Provides details about a labeling work team.
--
--
--
-- /See:/ 'workteam' smart constructor.
data Workteam = Workteam'{_worSubDomain ::
                          !(Maybe Text),
                          _worProductListingIds :: !(Maybe [Text]),
                          _worNotificationConfiguration ::
                          !(Maybe NotificationConfiguration),
                          _worCreateDate :: !(Maybe POSIX),
                          _worLastUpdatedDate :: !(Maybe POSIX),
                          _worWorkteamName :: !Text,
                          _worMemberDefinitions :: !(List1 MemberDefinition),
                          _worWorkteamARN :: !Text, _worDescription :: !Text}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Workteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'worSubDomain' - The URI of the labeling job's user interface. Workers open this URI to start labeling your data objects.
--
-- * 'worProductListingIds' - The Amazon Marketplace identifier for a vendor's work team.
--
-- * 'worNotificationConfiguration' - Configures SNS notifications of available or expiring work items for work teams.
--
-- * 'worCreateDate' - The date and time that the work team was created (timestamp).
--
-- * 'worLastUpdatedDate' - The date and time that the work team was last updated (timestamp).
--
-- * 'worWorkteamName' - The name of the work team.
--
-- * 'worMemberDefinitions' - The Amazon Cognito user groups that make up the work team.
--
-- * 'worWorkteamARN' - The Amazon Resource Name (ARN) that identifies the work team.
--
-- * 'worDescription' - A description of the work team.
workteam
    :: Text -- ^ 'worWorkteamName'
    -> NonEmpty MemberDefinition -- ^ 'worMemberDefinitions'
    -> Text -- ^ 'worWorkteamARN'
    -> Text -- ^ 'worDescription'
    -> Workteam
workteam pWorkteamName_ pMemberDefinitions_
  pWorkteamARN_ pDescription_
  = Workteam'{_worSubDomain = Nothing,
              _worProductListingIds = Nothing,
              _worNotificationConfiguration = Nothing,
              _worCreateDate = Nothing,
              _worLastUpdatedDate = Nothing,
              _worWorkteamName = pWorkteamName_,
              _worMemberDefinitions = _List1 # pMemberDefinitions_,
              _worWorkteamARN = pWorkteamARN_,
              _worDescription = pDescription_}

-- | The URI of the labeling job's user interface. Workers open this URI to start labeling your data objects.
worSubDomain :: Lens' Workteam (Maybe Text)
worSubDomain = lens _worSubDomain (\ s a -> s{_worSubDomain = a})

-- | The Amazon Marketplace identifier for a vendor's work team.
worProductListingIds :: Lens' Workteam [Text]
worProductListingIds = lens _worProductListingIds (\ s a -> s{_worProductListingIds = a}) . _Default . _Coerce

-- | Configures SNS notifications of available or expiring work items for work teams.
worNotificationConfiguration :: Lens' Workteam (Maybe NotificationConfiguration)
worNotificationConfiguration = lens _worNotificationConfiguration (\ s a -> s{_worNotificationConfiguration = a})

-- | The date and time that the work team was created (timestamp).
worCreateDate :: Lens' Workteam (Maybe UTCTime)
worCreateDate = lens _worCreateDate (\ s a -> s{_worCreateDate = a}) . mapping _Time

-- | The date and time that the work team was last updated (timestamp).
worLastUpdatedDate :: Lens' Workteam (Maybe UTCTime)
worLastUpdatedDate = lens _worLastUpdatedDate (\ s a -> s{_worLastUpdatedDate = a}) . mapping _Time

-- | The name of the work team.
worWorkteamName :: Lens' Workteam Text
worWorkteamName = lens _worWorkteamName (\ s a -> s{_worWorkteamName = a})

-- | The Amazon Cognito user groups that make up the work team.
worMemberDefinitions :: Lens' Workteam (NonEmpty MemberDefinition)
worMemberDefinitions = lens _worMemberDefinitions (\ s a -> s{_worMemberDefinitions = a}) . _List1

-- | The Amazon Resource Name (ARN) that identifies the work team.
worWorkteamARN :: Lens' Workteam Text
worWorkteamARN = lens _worWorkteamARN (\ s a -> s{_worWorkteamARN = a})

-- | A description of the work team.
worDescription :: Lens' Workteam Text
worDescription = lens _worDescription (\ s a -> s{_worDescription = a})

instance FromJSON Workteam where
        parseJSON
          = withObject "Workteam"
              (\ x ->
                 Workteam' <$>
                   (x .:? "SubDomain") <*>
                     (x .:? "ProductListingIds" .!= mempty)
                     <*> (x .:? "NotificationConfiguration")
                     <*> (x .:? "CreateDate")
                     <*> (x .:? "LastUpdatedDate")
                     <*> (x .: "WorkteamName")
                     <*> (x .: "MemberDefinitions")
                     <*> (x .: "WorkteamArn")
                     <*> (x .: "Description"))

instance Hashable Workteam where

instance NFData Workteam where
