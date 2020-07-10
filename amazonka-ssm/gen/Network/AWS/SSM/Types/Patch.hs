{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Patch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Patch where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents metadata about a patch.
--
--
--
-- /See:/ 'patch' smart constructor.
data Patch = Patch'{_pVendor :: !(Maybe Text),
                    _pMsrcSeverity :: !(Maybe Text),
                    _pProductFamily :: !(Maybe Text),
                    _pClassification :: !(Maybe Text),
                    _pMsrcNumber :: !(Maybe Text),
                    _pLanguage :: !(Maybe Text),
                    _pKbNumber :: !(Maybe Text),
                    _pContentURL :: !(Maybe Text), _pId :: !(Maybe Text),
                    _pReleaseDate :: !(Maybe POSIX),
                    _pTitle :: !(Maybe Text), _pProduct :: !(Maybe Text),
                    _pDescription :: !(Maybe Text)}
               deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Patch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pVendor' - The name of the vendor providing the patch.
--
-- * 'pMsrcSeverity' - The severity of the patch (for example Critical, Important, Moderate).
--
-- * 'pProductFamily' - The product family the patch is applicable for (for example, Windows).
--
-- * 'pClassification' - The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
--
-- * 'pMsrcNumber' - The ID of the MSRC bulletin the patch is related to.
--
-- * 'pLanguage' - The language of the patch if it's language-specific.
--
-- * 'pKbNumber' - The Microsoft Knowledge Base ID of the patch.
--
-- * 'pContentURL' - The URL where more information can be obtained about the patch.
--
-- * 'pId' - The ID of the patch (this is different than the Microsoft Knowledge Base ID).
--
-- * 'pReleaseDate' - The date the patch was released.
--
-- * 'pTitle' - The title of the patch.
--
-- * 'pProduct' - The specific product the patch is applicable for (for example, WindowsServer2016).
--
-- * 'pDescription' - The description of the patch.
patch
    :: Patch
patch
  = Patch'{_pVendor = Nothing,
           _pMsrcSeverity = Nothing, _pProductFamily = Nothing,
           _pClassification = Nothing, _pMsrcNumber = Nothing,
           _pLanguage = Nothing, _pKbNumber = Nothing,
           _pContentURL = Nothing, _pId = Nothing,
           _pReleaseDate = Nothing, _pTitle = Nothing,
           _pProduct = Nothing, _pDescription = Nothing}

-- | The name of the vendor providing the patch.
pVendor :: Lens' Patch (Maybe Text)
pVendor = lens _pVendor (\ s a -> s{_pVendor = a})

-- | The severity of the patch (for example Critical, Important, Moderate).
pMsrcSeverity :: Lens' Patch (Maybe Text)
pMsrcSeverity = lens _pMsrcSeverity (\ s a -> s{_pMsrcSeverity = a})

-- | The product family the patch is applicable for (for example, Windows).
pProductFamily :: Lens' Patch (Maybe Text)
pProductFamily = lens _pProductFamily (\ s a -> s{_pProductFamily = a})

-- | The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
pClassification :: Lens' Patch (Maybe Text)
pClassification = lens _pClassification (\ s a -> s{_pClassification = a})

-- | The ID of the MSRC bulletin the patch is related to.
pMsrcNumber :: Lens' Patch (Maybe Text)
pMsrcNumber = lens _pMsrcNumber (\ s a -> s{_pMsrcNumber = a})

-- | The language of the patch if it's language-specific.
pLanguage :: Lens' Patch (Maybe Text)
pLanguage = lens _pLanguage (\ s a -> s{_pLanguage = a})

-- | The Microsoft Knowledge Base ID of the patch.
pKbNumber :: Lens' Patch (Maybe Text)
pKbNumber = lens _pKbNumber (\ s a -> s{_pKbNumber = a})

-- | The URL where more information can be obtained about the patch.
pContentURL :: Lens' Patch (Maybe Text)
pContentURL = lens _pContentURL (\ s a -> s{_pContentURL = a})

-- | The ID of the patch (this is different than the Microsoft Knowledge Base ID).
pId :: Lens' Patch (Maybe Text)
pId = lens _pId (\ s a -> s{_pId = a})

-- | The date the patch was released.
pReleaseDate :: Lens' Patch (Maybe UTCTime)
pReleaseDate = lens _pReleaseDate (\ s a -> s{_pReleaseDate = a}) . mapping _Time

-- | The title of the patch.
pTitle :: Lens' Patch (Maybe Text)
pTitle = lens _pTitle (\ s a -> s{_pTitle = a})

-- | The specific product the patch is applicable for (for example, WindowsServer2016).
pProduct :: Lens' Patch (Maybe Text)
pProduct = lens _pProduct (\ s a -> s{_pProduct = a})

-- | The description of the patch.
pDescription :: Lens' Patch (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

instance FromJSON Patch where
        parseJSON
          = withObject "Patch"
              (\ x ->
                 Patch' <$>
                   (x .:? "Vendor") <*> (x .:? "MsrcSeverity") <*>
                     (x .:? "ProductFamily")
                     <*> (x .:? "Classification")
                     <*> (x .:? "MsrcNumber")
                     <*> (x .:? "Language")
                     <*> (x .:? "KbNumber")
                     <*> (x .:? "ContentUrl")
                     <*> (x .:? "Id")
                     <*> (x .:? "ReleaseDate")
                     <*> (x .:? "Title")
                     <*> (x .:? "Product")
                     <*> (x .:? "Description"))

instance Hashable Patch where

instance NFData Patch where
