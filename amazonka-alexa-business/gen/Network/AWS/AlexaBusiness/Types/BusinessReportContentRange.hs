{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.BusinessReportContentRange where

import Network.AWS.AlexaBusiness.Types.BusinessReportInterval
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The content range of the report.
--
--
--
-- /See:/ 'businessReportContentRange' smart constructor.
newtype BusinessReportContentRange = BusinessReportContentRange'{_brcrInterval
                                                                 ::
                                                                 Maybe
                                                                   BusinessReportInterval}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'BusinessReportContentRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brcrInterval' - The interval of the content range.
businessReportContentRange
    :: BusinessReportContentRange
businessReportContentRange
  = BusinessReportContentRange'{_brcrInterval =
                                  Nothing}

-- | The interval of the content range.
brcrInterval :: Lens' BusinessReportContentRange (Maybe BusinessReportInterval)
brcrInterval = lens _brcrInterval (\ s a -> s{_brcrInterval = a})

instance FromJSON BusinessReportContentRange where
        parseJSON
          = withObject "BusinessReportContentRange"
              (\ x ->
                 BusinessReportContentRange' <$> (x .:? "Interval"))

instance Hashable BusinessReportContentRange where

instance NFData BusinessReportContentRange where

instance ToJSON BusinessReportContentRange where
        toJSON BusinessReportContentRange'{..}
          = object
              (catMaybes [("Interval" .=) <$> _brcrInterval])
