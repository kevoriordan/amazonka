{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.CapacityProvider where

import Network.AWS.ECS.Types.AutoScalingGroupProvider
import Network.AWS.ECS.Types.CapacityProviderStatus
import Network.AWS.ECS.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of a capacity provider.
--
--
--
-- /See:/ 'capacityProvider' smart constructor.
data CapacityProvider = CapacityProvider'{_cpStatus
                                          :: !(Maybe CapacityProviderStatus),
                                          _cpAutoScalingGroupProvider ::
                                          !(Maybe AutoScalingGroupProvider),
                                          _cpName :: !(Maybe Text),
                                          _cpCapacityProviderARN ::
                                          !(Maybe Text),
                                          _cpTags :: !(Maybe [Tag])}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CapacityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpStatus' - The current status of the capacity provider. Only capacity providers in an @ACTIVE@ state can be used in a cluster.
--
-- * 'cpAutoScalingGroupProvider' - The Auto Scaling group settings for the capacity provider.
--
-- * 'cpName' - The name of the capacity provider.
--
-- * 'cpCapacityProviderARN' - The Amazon Resource Name (ARN) that identifies the capacity provider.
--
-- * 'cpTags' - The metadata that you apply to the capacity provider to help you categorize and organize it. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
capacityProvider
    :: CapacityProvider
capacityProvider
  = CapacityProvider'{_cpStatus = Nothing,
                      _cpAutoScalingGroupProvider = Nothing,
                      _cpName = Nothing, _cpCapacityProviderARN = Nothing,
                      _cpTags = Nothing}

-- | The current status of the capacity provider. Only capacity providers in an @ACTIVE@ state can be used in a cluster.
cpStatus :: Lens' CapacityProvider (Maybe CapacityProviderStatus)
cpStatus = lens _cpStatus (\ s a -> s{_cpStatus = a})

-- | The Auto Scaling group settings for the capacity provider.
cpAutoScalingGroupProvider :: Lens' CapacityProvider (Maybe AutoScalingGroupProvider)
cpAutoScalingGroupProvider = lens _cpAutoScalingGroupProvider (\ s a -> s{_cpAutoScalingGroupProvider = a})

-- | The name of the capacity provider.
cpName :: Lens' CapacityProvider (Maybe Text)
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | The Amazon Resource Name (ARN) that identifies the capacity provider.
cpCapacityProviderARN :: Lens' CapacityProvider (Maybe Text)
cpCapacityProviderARN = lens _cpCapacityProviderARN (\ s a -> s{_cpCapacityProviderARN = a})

-- | The metadata that you apply to the capacity provider to help you categorize and organize it. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
cpTags :: Lens' CapacityProvider [Tag]
cpTags = lens _cpTags (\ s a -> s{_cpTags = a}) . _Default . _Coerce

instance FromJSON CapacityProvider where
        parseJSON
          = withObject "CapacityProvider"
              (\ x ->
                 CapacityProvider' <$>
                   (x .:? "status") <*>
                     (x .:? "autoScalingGroupProvider")
                     <*> (x .:? "name")
                     <*> (x .:? "capacityProviderArn")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable CapacityProvider where

instance NFData CapacityProvider where
