{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.SuspendedProcess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.SuspendedProcess where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an automatic scaling process that has been suspended. For more information, see 'ProcessType' .
--
--
--
-- /See:/ 'suspendedProcess' smart constructor.
data SuspendedProcess = SuspendedProcess'{_spProcessName
                                          :: !(Maybe Text),
                                          _spSuspensionReason :: !(Maybe Text)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuspendedProcess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spProcessName' - The name of the suspended process.
--
-- * 'spSuspensionReason' - The reason that the process was suspended.
suspendedProcess
    :: SuspendedProcess
suspendedProcess
  = SuspendedProcess'{_spProcessName = Nothing,
                      _spSuspensionReason = Nothing}

-- | The name of the suspended process.
spProcessName :: Lens' SuspendedProcess (Maybe Text)
spProcessName = lens _spProcessName (\ s a -> s{_spProcessName = a})

-- | The reason that the process was suspended.
spSuspensionReason :: Lens' SuspendedProcess (Maybe Text)
spSuspensionReason = lens _spSuspensionReason (\ s a -> s{_spSuspensionReason = a})

instance FromXML SuspendedProcess where
        parseXML x
          = SuspendedProcess' <$>
              (x .@? "ProcessName") <*> (x .@? "SuspensionReason")

instance Hashable SuspendedProcess where

instance NFData SuspendedProcess where
