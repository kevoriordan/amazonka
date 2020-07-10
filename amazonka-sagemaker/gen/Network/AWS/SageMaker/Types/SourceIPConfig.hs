{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SourceIPConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SourceIPConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ). Used to create an allow list of IP addresses for a private workforce. For more information, see .
--
--
--
-- /See:/ 'sourceIPConfig' smart constructor.
newtype SourceIPConfig = SourceIPConfig'{_sicCidrs ::
                                         [Text]}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceIPConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sicCidrs' - A list of one to four <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Classless Inter-Domain Routing> (CIDR) values. Maximum: Four CIDR values
sourceIPConfig
    :: SourceIPConfig
sourceIPConfig = SourceIPConfig'{_sicCidrs = mempty}

-- | A list of one to four <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Classless Inter-Domain Routing> (CIDR) values. Maximum: Four CIDR values
sicCidrs :: Lens' SourceIPConfig [Text]
sicCidrs = lens _sicCidrs (\ s a -> s{_sicCidrs = a}) . _Coerce

instance FromJSON SourceIPConfig where
        parseJSON
          = withObject "SourceIPConfig"
              (\ x ->
                 SourceIPConfig' <$> (x .:? "Cidrs" .!= mempty))

instance Hashable SourceIPConfig where

instance NFData SourceIPConfig where

instance ToJSON SourceIPConfig where
        toJSON SourceIPConfig'{..}
          = object (catMaybes [Just ("Cidrs" .= _sicCidrs)])
