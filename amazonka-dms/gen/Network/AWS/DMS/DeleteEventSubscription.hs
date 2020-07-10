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
-- Module      : Network.AWS.DMS.DeleteEventSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS DMS event subscription. 
--
--
module Network.AWS.DMS.DeleteEventSubscription
    (
    -- * Creating a Request
      deleteEventSubscription
    , DeleteEventSubscription
    -- * Request Lenses
    , desSubscriptionName

    -- * Destructuring the Response
    , deleteEventSubscriptionResponse
    , DeleteEventSubscriptionResponse
    -- * Response Lenses
    , dltevntsbscrptnrsEventSubscription
    , dltevntsbscrptnrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
--
--
-- /See:/ 'deleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'{_desSubscriptionName
                                                           :: Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DeleteEventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desSubscriptionName' - The name of the DMS event notification subscription to be deleted.
deleteEventSubscription
    :: Text -- ^ 'desSubscriptionName'
    -> DeleteEventSubscription
deleteEventSubscription pSubscriptionName_
  = DeleteEventSubscription'{_desSubscriptionName =
                               pSubscriptionName_}

-- | The name of the DMS event notification subscription to be deleted.
desSubscriptionName :: Lens' DeleteEventSubscription Text
desSubscriptionName = lens _desSubscriptionName (\ s a -> s{_desSubscriptionName = a})

instance AWSRequest DeleteEventSubscription where
        type Rs DeleteEventSubscription =
             DeleteEventSubscriptionResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DeleteEventSubscriptionResponse' <$>
                   (x .?> "EventSubscription") <*> (pure (fromEnum s)))

instance Hashable DeleteEventSubscription where

instance NFData DeleteEventSubscription where

instance ToHeaders DeleteEventSubscription where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DeleteEventSubscription" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEventSubscription where
        toJSON DeleteEventSubscription'{..}
          = object
              (catMaybes
                 [Just ("SubscriptionName" .= _desSubscriptionName)])

instance ToPath DeleteEventSubscription where
        toPath = const "/"

instance ToQuery DeleteEventSubscription where
        toQuery = const mempty

-- | 
--
--
--
-- /See:/ 'deleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'{_dltevntsbscrptnrsEventSubscription
                                                                        ::
                                                                        !(Maybe
                                                                            EventSubscription),
                                                                        _dltevntsbscrptnrsResponseStatus
                                                                        :: !Int}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'DeleteEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltevntsbscrptnrsEventSubscription' - The event subscription that was deleted.
--
-- * 'dltevntsbscrptnrsResponseStatus' - -- | The response status code.
deleteEventSubscriptionResponse
    :: Int -- ^ 'dltevntsbscrptnrsResponseStatus'
    -> DeleteEventSubscriptionResponse
deleteEventSubscriptionResponse pResponseStatus_
  = DeleteEventSubscriptionResponse'{_dltevntsbscrptnrsEventSubscription
                                       = Nothing,
                                     _dltevntsbscrptnrsResponseStatus =
                                       pResponseStatus_}

-- | The event subscription that was deleted.
dltevntsbscrptnrsEventSubscription :: Lens' DeleteEventSubscriptionResponse (Maybe EventSubscription)
dltevntsbscrptnrsEventSubscription = lens _dltevntsbscrptnrsEventSubscription (\ s a -> s{_dltevntsbscrptnrsEventSubscription = a})

-- | -- | The response status code.
dltevntsbscrptnrsResponseStatus :: Lens' DeleteEventSubscriptionResponse Int
dltevntsbscrptnrsResponseStatus = lens _dltevntsbscrptnrsResponseStatus (\ s a -> s{_dltevntsbscrptnrsResponseStatus = a})

instance NFData DeleteEventSubscriptionResponse where
