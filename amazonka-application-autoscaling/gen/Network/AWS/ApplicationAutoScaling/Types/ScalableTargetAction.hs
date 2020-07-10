{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the minimum and maximum capacity for a scheduled action.
--
--
--
-- /See:/ 'scalableTargetAction' smart constructor.
data ScalableTargetAction = ScalableTargetAction'{_staMaxCapacity
                                                  :: !(Maybe Int),
                                                  _staMinCapacity ::
                                                  !(Maybe Int)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalableTargetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staMaxCapacity' - The maximum capacity.
--
-- * 'staMinCapacity' - The minimum capacity. For Lambda provisioned concurrency, the minimum value allowed is 0. For all other resources, the minimum value allowed is 1.
scalableTargetAction
    :: ScalableTargetAction
scalableTargetAction
  = ScalableTargetAction'{_staMaxCapacity = Nothing,
                          _staMinCapacity = Nothing}

-- | The maximum capacity.
staMaxCapacity :: Lens' ScalableTargetAction (Maybe Int)
staMaxCapacity = lens _staMaxCapacity (\ s a -> s{_staMaxCapacity = a})

-- | The minimum capacity. For Lambda provisioned concurrency, the minimum value allowed is 0. For all other resources, the minimum value allowed is 1.
staMinCapacity :: Lens' ScalableTargetAction (Maybe Int)
staMinCapacity = lens _staMinCapacity (\ s a -> s{_staMinCapacity = a})

instance FromJSON ScalableTargetAction where
        parseJSON
          = withObject "ScalableTargetAction"
              (\ x ->
                 ScalableTargetAction' <$>
                   (x .:? "MaxCapacity") <*> (x .:? "MinCapacity"))

instance Hashable ScalableTargetAction where

instance NFData ScalableTargetAction where

instance ToJSON ScalableTargetAction where
        toJSON ScalableTargetAction'{..}
          = object
              (catMaybes
                 [("MaxCapacity" .=) <$> _staMaxCapacity,
                  ("MinCapacity" .=) <$> _staMinCapacity])
