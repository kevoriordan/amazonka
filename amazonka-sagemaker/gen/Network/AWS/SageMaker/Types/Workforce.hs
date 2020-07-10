{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Workforce
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Workforce where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.SourceIPConfig

-- | A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
--
--
--
-- /See:/ 'workforce' smart constructor.
data Workforce = Workforce'{_wSourceIPConfig ::
                            !(Maybe SourceIPConfig),
                            _wLastUpdatedDate :: !(Maybe POSIX),
                            _wWorkforceName :: !Text, _wWorkforceARN :: !Text}
                   deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Workforce' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wSourceIPConfig' - A list of one to four IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to be added to the workforce allow list.
--
-- * 'wLastUpdatedDate' - The most recent date that was used to successfully add one or more IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to a private workforce's allow list.
--
-- * 'wWorkforceName' - The name of the private workforce whose access you want to restrict. @WorkforceName@ is automatically set to @default@ when a workforce is created and cannot be modified. 
--
-- * 'wWorkforceARN' - The Amazon Resource Name (ARN) of the private workforce.
workforce
    :: Text -- ^ 'wWorkforceName'
    -> Text -- ^ 'wWorkforceARN'
    -> Workforce
workforce pWorkforceName_ pWorkforceARN_
  = Workforce'{_wSourceIPConfig = Nothing,
               _wLastUpdatedDate = Nothing,
               _wWorkforceName = pWorkforceName_,
               _wWorkforceARN = pWorkforceARN_}

-- | A list of one to four IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to be added to the workforce allow list.
wSourceIPConfig :: Lens' Workforce (Maybe SourceIPConfig)
wSourceIPConfig = lens _wSourceIPConfig (\ s a -> s{_wSourceIPConfig = a})

-- | The most recent date that was used to successfully add one or more IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) to a private workforce's allow list.
wLastUpdatedDate :: Lens' Workforce (Maybe UTCTime)
wLastUpdatedDate = lens _wLastUpdatedDate (\ s a -> s{_wLastUpdatedDate = a}) . mapping _Time

-- | The name of the private workforce whose access you want to restrict. @WorkforceName@ is automatically set to @default@ when a workforce is created and cannot be modified. 
wWorkforceName :: Lens' Workforce Text
wWorkforceName = lens _wWorkforceName (\ s a -> s{_wWorkforceName = a})

-- | The Amazon Resource Name (ARN) of the private workforce.
wWorkforceARN :: Lens' Workforce Text
wWorkforceARN = lens _wWorkforceARN (\ s a -> s{_wWorkforceARN = a})

instance FromJSON Workforce where
        parseJSON
          = withObject "Workforce"
              (\ x ->
                 Workforce' <$>
                   (x .:? "SourceIpConfig") <*>
                     (x .:? "LastUpdatedDate")
                     <*> (x .: "WorkforceName")
                     <*> (x .: "WorkforceArn"))

instance Hashable Workforce where

instance NFData Workforce where
