{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Subscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.Subscription where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.AutoRenew
import Network.AWS.Shield.Types.Limit

-- | Information about the AWS Shield Advanced subscription for an account.
--
--
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'{_sTimeCommitmentInSeconds
                                  :: !(Maybe Nat),
                                  _sStartTime :: !(Maybe POSIX),
                                  _sLimits :: !(Maybe [Limit]),
                                  _sAutoRenew :: !(Maybe AutoRenew),
                                  _sEndTime :: !(Maybe POSIX)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTimeCommitmentInSeconds' - The length, in seconds, of the AWS Shield Advanced subscription for the account.
--
-- * 'sStartTime' - The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- * 'sLimits' - Specifies how many protections of a given type you can create.
--
-- * 'sAutoRenew' - If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
--
-- * 'sEndTime' - The date and time your subscription will end.
subscription
    :: Subscription
subscription
  = Subscription'{_sTimeCommitmentInSeconds = Nothing,
                  _sStartTime = Nothing, _sLimits = Nothing,
                  _sAutoRenew = Nothing, _sEndTime = Nothing}

-- | The length, in seconds, of the AWS Shield Advanced subscription for the account.
sTimeCommitmentInSeconds :: Lens' Subscription (Maybe Natural)
sTimeCommitmentInSeconds = lens _sTimeCommitmentInSeconds (\ s a -> s{_sTimeCommitmentInSeconds = a}) . mapping _Nat

-- | The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
sStartTime :: Lens' Subscription (Maybe UTCTime)
sStartTime = lens _sStartTime (\ s a -> s{_sStartTime = a}) . mapping _Time

-- | Specifies how many protections of a given type you can create.
sLimits :: Lens' Subscription [Limit]
sLimits = lens _sLimits (\ s a -> s{_sLimits = a}) . _Default . _Coerce

-- | If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
sAutoRenew :: Lens' Subscription (Maybe AutoRenew)
sAutoRenew = lens _sAutoRenew (\ s a -> s{_sAutoRenew = a})

-- | The date and time your subscription will end.
sEndTime :: Lens' Subscription (Maybe UTCTime)
sEndTime = lens _sEndTime (\ s a -> s{_sEndTime = a}) . mapping _Time

instance FromJSON Subscription where
        parseJSON
          = withObject "Subscription"
              (\ x ->
                 Subscription' <$>
                   (x .:? "TimeCommitmentInSeconds") <*>
                     (x .:? "StartTime")
                     <*> (x .:? "Limits" .!= mempty)
                     <*> (x .:? "AutoRenew")
                     <*> (x .:? "EndTime"))

instance Hashable Subscription where

instance NFData Subscription where
