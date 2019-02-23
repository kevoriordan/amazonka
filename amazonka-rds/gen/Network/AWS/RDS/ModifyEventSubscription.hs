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
-- Module      : Network.AWS.RDS.ModifyEventSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing RDS event notification subscription. Note that you can't modify the source identifiers using this call; to change source identifiers for a subscription, use the 'AddSourceIdentifierToSubscription' and 'RemoveSourceIdentifierFromSubscription' calls.
--
--
-- You can see a list of the event categories for a given SourceType in the <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events> topic in the /Amazon RDS User Guide/ or by using the __DescribeEventCategories__ action.
--
module Network.AWS.RDS.ModifyEventSubscription
    (
    -- * Creating a Request
      modifyEventSubscription
    , ModifyEventSubscription
    -- * Request Lenses
    , mesSNSTopicARN
    , mesEnabled
    , mesSourceType
    , mesEventCategories
    , mesSubscriptionName

    -- * Destructuring the Response
    , modifyEventSubscriptionResponse
    , ModifyEventSubscriptionResponse
    -- * Response Lenses
    , mesrsEventSubscription
    , mesrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifyEventSubscription' smart constructor.
data ModifyEventSubscription = ModifyEventSubscription'
  { _mesSNSTopicARN      :: !(Maybe Text)
  , _mesEnabled          :: !(Maybe Bool)
  , _mesSourceType       :: !(Maybe Text)
  , _mesEventCategories  :: !(Maybe [Text])
  , _mesSubscriptionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyEventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mesSNSTopicARN' - The Amazon Resource Name (ARN) of the SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it.
--
-- * 'mesEnabled' - A Boolean value; set to __true__ to activate the subscription.
--
-- * 'mesSourceType' - The type of source that is generating the events. For example, if you want to be notified of events generated by a DB instance, you would set this parameter to db-instance. if this value is not specified, all events are returned. Valid values: db-instance | db-parameter-group | db-security-group | db-snapshot
--
-- * 'mesEventCategories' - A list of event categories for a SourceType that you want to subscribe to. You can see a list of the categories for a given SourceType in the <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events> topic in the /Amazon RDS User Guide/ or by using the __DescribeEventCategories__ action.
--
-- * 'mesSubscriptionName' - The name of the RDS event notification subscription.
modifyEventSubscription
    :: Text -- ^ 'mesSubscriptionName'
    -> ModifyEventSubscription
modifyEventSubscription pSubscriptionName_ =
  ModifyEventSubscription'
    { _mesSNSTopicARN = Nothing
    , _mesEnabled = Nothing
    , _mesSourceType = Nothing
    , _mesEventCategories = Nothing
    , _mesSubscriptionName = pSubscriptionName_
    }


-- | The Amazon Resource Name (ARN) of the SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it.
mesSNSTopicARN :: Lens' ModifyEventSubscription (Maybe Text)
mesSNSTopicARN = lens _mesSNSTopicARN (\ s a -> s{_mesSNSTopicARN = a})

-- | A Boolean value; set to __true__ to activate the subscription.
mesEnabled :: Lens' ModifyEventSubscription (Maybe Bool)
mesEnabled = lens _mesEnabled (\ s a -> s{_mesEnabled = a})

-- | The type of source that is generating the events. For example, if you want to be notified of events generated by a DB instance, you would set this parameter to db-instance. if this value is not specified, all events are returned. Valid values: db-instance | db-parameter-group | db-security-group | db-snapshot
mesSourceType :: Lens' ModifyEventSubscription (Maybe Text)
mesSourceType = lens _mesSourceType (\ s a -> s{_mesSourceType = a})

-- | A list of event categories for a SourceType that you want to subscribe to. You can see a list of the categories for a given SourceType in the <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.html Events> topic in the /Amazon RDS User Guide/ or by using the __DescribeEventCategories__ action.
mesEventCategories :: Lens' ModifyEventSubscription [Text]
mesEventCategories = lens _mesEventCategories (\ s a -> s{_mesEventCategories = a}) . _Default . _Coerce

-- | The name of the RDS event notification subscription.
mesSubscriptionName :: Lens' ModifyEventSubscription Text
mesSubscriptionName = lens _mesSubscriptionName (\ s a -> s{_mesSubscriptionName = a})

instance AWSRequest ModifyEventSubscription where
        type Rs ModifyEventSubscription =
             ModifyEventSubscriptionResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "ModifyEventSubscriptionResult"
              (\ s h x ->
                 ModifyEventSubscriptionResponse' <$>
                   (x .@? "EventSubscription") <*> (pure (fromEnum s)))

instance Hashable ModifyEventSubscription where

instance NFData ModifyEventSubscription where

instance ToHeaders ModifyEventSubscription where
        toHeaders = const mempty

instance ToPath ModifyEventSubscription where
        toPath = const "/"

instance ToQuery ModifyEventSubscription where
        toQuery ModifyEventSubscription'{..}
          = mconcat
              ["Action" =:
                 ("ModifyEventSubscription" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "SnsTopicArn" =: _mesSNSTopicARN,
               "Enabled" =: _mesEnabled,
               "SourceType" =: _mesSourceType,
               "EventCategories" =:
                 toQuery
                   (toQueryList "EventCategory" <$>
                      _mesEventCategories),
               "SubscriptionName" =: _mesSubscriptionName]

-- | /See:/ 'modifyEventSubscriptionResponse' smart constructor.
data ModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse'
  { _mesrsEventSubscription :: !(Maybe EventSubscription)
  , _mesrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mesrsEventSubscription' - Undocumented member.
--
-- * 'mesrsResponseStatus' - -- | The response status code.
modifyEventSubscriptionResponse
    :: Int -- ^ 'mesrsResponseStatus'
    -> ModifyEventSubscriptionResponse
modifyEventSubscriptionResponse pResponseStatus_ =
  ModifyEventSubscriptionResponse'
    {_mesrsEventSubscription = Nothing, _mesrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
mesrsEventSubscription :: Lens' ModifyEventSubscriptionResponse (Maybe EventSubscription)
mesrsEventSubscription = lens _mesrsEventSubscription (\ s a -> s{_mesrsEventSubscription = a})

-- | -- | The response status code.
mesrsResponseStatus :: Lens' ModifyEventSubscriptionResponse Int
mesrsResponseStatus = lens _mesrsResponseStatus (\ s a -> s{_mesrsResponseStatus = a})

instance NFData ModifyEventSubscriptionResponse where
