{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.TemporalFilterSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
import Network.AWS.MediaLive.Types.TemporalFilterStrength
import Network.AWS.Prelude

-- | Temporal Filter Settings
--
-- /See:/ 'temporalFilterSettings' smart constructor.
data TemporalFilterSettings = TemporalFilterSettings'{_tfsStrength
                                                      ::
                                                      !(Maybe
                                                          TemporalFilterStrength),
                                                      _tfsPostFilterSharpening
                                                      ::
                                                      !(Maybe
                                                          TemporalFilterPostFilterSharpening)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'TemporalFilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfsStrength' - Filter strength. A higher value produces stronger filtering.
--
-- * 'tfsPostFilterSharpening' - If set to "ENABLED," applies post-filter sharpening to improve visual quality. This is most beneficial when using a noisy or compressed input source and low output bitrates.
temporalFilterSettings
    :: TemporalFilterSettings
temporalFilterSettings
  = TemporalFilterSettings'{_tfsStrength = Nothing,
                            _tfsPostFilterSharpening = Nothing}

-- | Filter strength. A higher value produces stronger filtering.
tfsStrength :: Lens' TemporalFilterSettings (Maybe TemporalFilterStrength)
tfsStrength = lens _tfsStrength (\ s a -> s{_tfsStrength = a})

-- | If set to "ENABLED," applies post-filter sharpening to improve visual quality. This is most beneficial when using a noisy or compressed input source and low output bitrates.
tfsPostFilterSharpening :: Lens' TemporalFilterSettings (Maybe TemporalFilterPostFilterSharpening)
tfsPostFilterSharpening = lens _tfsPostFilterSharpening (\ s a -> s{_tfsPostFilterSharpening = a})

instance FromJSON TemporalFilterSettings where
        parseJSON
          = withObject "TemporalFilterSettings"
              (\ x ->
                 TemporalFilterSettings' <$>
                   (x .:? "strength") <*>
                     (x .:? "postFilterSharpening"))

instance Hashable TemporalFilterSettings where

instance NFData TemporalFilterSettings where

instance ToJSON TemporalFilterSettings where
        toJSON TemporalFilterSettings'{..}
          = object
              (catMaybes
                 [("strength" .=) <$> _tfsStrength,
                  ("postFilterSharpening" .=) <$>
                    _tfsPostFilterSharpening])
