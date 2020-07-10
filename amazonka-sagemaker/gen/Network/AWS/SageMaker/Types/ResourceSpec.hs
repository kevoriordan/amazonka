{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceSpec
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ResourceSpec where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AppInstanceType

-- | The instance type and the Amazon Resource Name (ARN) of the image created on the instance. The ARN is stored as metadata in Amazon SageMaker Studio notebooks.
--
--
--
-- /See:/ 'resourceSpec' smart constructor.
data ResourceSpec = ResourceSpec'{_rsInstanceType ::
                                  !(Maybe AppInstanceType),
                                  _rsSageMakerImageARN :: !(Maybe Text)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsInstanceType' - The instance type.
--
-- * 'rsSageMakerImageARN' - The Amazon Resource Name (ARN) of the image created on the instance.
resourceSpec
    :: ResourceSpec
resourceSpec
  = ResourceSpec'{_rsInstanceType = Nothing,
                  _rsSageMakerImageARN = Nothing}

-- | The instance type.
rsInstanceType :: Lens' ResourceSpec (Maybe AppInstanceType)
rsInstanceType = lens _rsInstanceType (\ s a -> s{_rsInstanceType = a})

-- | The Amazon Resource Name (ARN) of the image created on the instance.
rsSageMakerImageARN :: Lens' ResourceSpec (Maybe Text)
rsSageMakerImageARN = lens _rsSageMakerImageARN (\ s a -> s{_rsSageMakerImageARN = a})

instance FromJSON ResourceSpec where
        parseJSON
          = withObject "ResourceSpec"
              (\ x ->
                 ResourceSpec' <$>
                   (x .:? "InstanceType") <*>
                     (x .:? "SageMakerImageArn"))

instance Hashable ResourceSpec where

instance NFData ResourceSpec where

instance ToJSON ResourceSpec where
        toJSON ResourceSpec'{..}
          = object
              (catMaybes
                 [("InstanceType" .=) <$> _rsInstanceType,
                  ("SageMakerImageArn" .=) <$> _rsSageMakerImageARN])
