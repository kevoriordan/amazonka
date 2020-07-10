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
-- Module      : Network.AWS.SageMaker.DescribeHumanTaskUi
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the requested human task user interface.
--
--
module Network.AWS.SageMaker.DescribeHumanTaskUi
    (
    -- * Creating a Request
      describeHumanTaskUi
    , DescribeHumanTaskUi
    -- * Request Lenses
    , dhtuHumanTaskUiName

    -- * Destructuring the Response
    , describeHumanTaskUiResponse
    , DescribeHumanTaskUiResponse
    -- * Response Lenses
    , dhtursResponseStatus
    , dhtursHumanTaskUiARN
    , dhtursHumanTaskUiName
    , dhtursCreationTime
    , dhtursUiTemplate
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeHumanTaskUi' smart constructor.
newtype DescribeHumanTaskUi = DescribeHumanTaskUi'{_dhtuHumanTaskUiName
                                                   :: Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DescribeHumanTaskUi' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhtuHumanTaskUiName' - The name of the human task user interface you want information about.
describeHumanTaskUi
    :: Text -- ^ 'dhtuHumanTaskUiName'
    -> DescribeHumanTaskUi
describeHumanTaskUi pHumanTaskUiName_
  = DescribeHumanTaskUi'{_dhtuHumanTaskUiName =
                           pHumanTaskUiName_}

-- | The name of the human task user interface you want information about.
dhtuHumanTaskUiName :: Lens' DescribeHumanTaskUi Text
dhtuHumanTaskUiName = lens _dhtuHumanTaskUiName (\ s a -> s{_dhtuHumanTaskUiName = a})

instance AWSRequest DescribeHumanTaskUi where
        type Rs DescribeHumanTaskUi =
             DescribeHumanTaskUiResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeHumanTaskUiResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "HumanTaskUiArn") <*>
                     (x .:> "HumanTaskUiName")
                     <*> (x .:> "CreationTime")
                     <*> (x .:> "UiTemplate"))

instance Hashable DescribeHumanTaskUi where

instance NFData DescribeHumanTaskUi where

instance ToHeaders DescribeHumanTaskUi where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeHumanTaskUi" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeHumanTaskUi where
        toJSON DescribeHumanTaskUi'{..}
          = object
              (catMaybes
                 [Just ("HumanTaskUiName" .= _dhtuHumanTaskUiName)])

instance ToPath DescribeHumanTaskUi where
        toPath = const "/"

instance ToQuery DescribeHumanTaskUi where
        toQuery = const mempty

-- | /See:/ 'describeHumanTaskUiResponse' smart constructor.
data DescribeHumanTaskUiResponse = DescribeHumanTaskUiResponse'{_dhtursResponseStatus
                                                                :: !Int,
                                                                _dhtursHumanTaskUiARN
                                                                :: !Text,
                                                                _dhtursHumanTaskUiName
                                                                :: !Text,
                                                                _dhtursCreationTime
                                                                :: !POSIX,
                                                                _dhtursUiTemplate
                                                                ::
                                                                !UiTemplateInfo}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'DescribeHumanTaskUiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhtursResponseStatus' - -- | The response status code.
--
-- * 'dhtursHumanTaskUiARN' - The Amazon Resource Name (ARN) of the human task user interface.
--
-- * 'dhtursHumanTaskUiName' - The name of the human task user interface.
--
-- * 'dhtursCreationTime' - The timestamp when the human task user interface was created.
--
-- * 'dhtursUiTemplate' - Undocumented member.
describeHumanTaskUiResponse
    :: Int -- ^ 'dhtursResponseStatus'
    -> Text -- ^ 'dhtursHumanTaskUiARN'
    -> Text -- ^ 'dhtursHumanTaskUiName'
    -> UTCTime -- ^ 'dhtursCreationTime'
    -> UiTemplateInfo -- ^ 'dhtursUiTemplate'
    -> DescribeHumanTaskUiResponse
describeHumanTaskUiResponse pResponseStatus_
  pHumanTaskUiARN_ pHumanTaskUiName_ pCreationTime_
  pUiTemplate_
  = DescribeHumanTaskUiResponse'{_dhtursResponseStatus
                                   = pResponseStatus_,
                                 _dhtursHumanTaskUiARN = pHumanTaskUiARN_,
                                 _dhtursHumanTaskUiName = pHumanTaskUiName_,
                                 _dhtursCreationTime = _Time # pCreationTime_,
                                 _dhtursUiTemplate = pUiTemplate_}

-- | -- | The response status code.
dhtursResponseStatus :: Lens' DescribeHumanTaskUiResponse Int
dhtursResponseStatus = lens _dhtursResponseStatus (\ s a -> s{_dhtursResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the human task user interface.
dhtursHumanTaskUiARN :: Lens' DescribeHumanTaskUiResponse Text
dhtursHumanTaskUiARN = lens _dhtursHumanTaskUiARN (\ s a -> s{_dhtursHumanTaskUiARN = a})

-- | The name of the human task user interface.
dhtursHumanTaskUiName :: Lens' DescribeHumanTaskUiResponse Text
dhtursHumanTaskUiName = lens _dhtursHumanTaskUiName (\ s a -> s{_dhtursHumanTaskUiName = a})

-- | The timestamp when the human task user interface was created.
dhtursCreationTime :: Lens' DescribeHumanTaskUiResponse UTCTime
dhtursCreationTime = lens _dhtursCreationTime (\ s a -> s{_dhtursCreationTime = a}) . _Time

-- | Undocumented member.
dhtursUiTemplate :: Lens' DescribeHumanTaskUiResponse UiTemplateInfo
dhtursUiTemplate = lens _dhtursUiTemplate (\ s a -> s{_dhtursUiTemplate = a})

instance NFData DescribeHumanTaskUiResponse where
