{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.VoiceId
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.VoiceId (
  VoiceId (
    ..
    , Aditi
    , Amy
    , Astrid
    , Bianca
    , Brian
    , Camila
    , Carla
    , Carmen
    , Celine
    , Chantal
    , Conchita
    , Cristiano
    , Dora
    , Emma
    , Enrique
    , Ewa
    , Filiz
    , Geraint
    , Giorgio
    , Gwyneth
    , Hans
    , Ines
    , Ivy
    , Jacek
    , Jan
    , Joanna
    , Joey
    , Justin
    , Karl
    , Kendra
    , Kimberly
    , Lea
    , Liv
    , Lotte
    , Lucia
    , Lupe
    , Mads
    , Maja
    , Marlene
    , Mathieu
    , Matthew
    , Maxim
    , Mia
    , Miguel
    , Mizuki
    , Naja
    , Nicole
    , Penelope
    , Raveena
    , Ricardo
    , Ruben
    , Russell
    , Salli
    , Seoyeon
    , Takumi
    , Tatyana
    , Vicki
    , Vitoria
    , Zeina
    , Zhiyu
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data VoiceId = VoiceId' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Aditi :: VoiceId
pattern Aditi = VoiceId' "Aditi"

pattern Amy :: VoiceId
pattern Amy = VoiceId' "Amy"

pattern Astrid :: VoiceId
pattern Astrid = VoiceId' "Astrid"

pattern Bianca :: VoiceId
pattern Bianca = VoiceId' "Bianca"

pattern Brian :: VoiceId
pattern Brian = VoiceId' "Brian"

pattern Camila :: VoiceId
pattern Camila = VoiceId' "Camila"

pattern Carla :: VoiceId
pattern Carla = VoiceId' "Carla"

pattern Carmen :: VoiceId
pattern Carmen = VoiceId' "Carmen"

pattern Celine :: VoiceId
pattern Celine = VoiceId' "Celine"

pattern Chantal :: VoiceId
pattern Chantal = VoiceId' "Chantal"

pattern Conchita :: VoiceId
pattern Conchita = VoiceId' "Conchita"

pattern Cristiano :: VoiceId
pattern Cristiano = VoiceId' "Cristiano"

pattern Dora :: VoiceId
pattern Dora = VoiceId' "Dora"

pattern Emma :: VoiceId
pattern Emma = VoiceId' "Emma"

pattern Enrique :: VoiceId
pattern Enrique = VoiceId' "Enrique"

pattern Ewa :: VoiceId
pattern Ewa = VoiceId' "Ewa"

pattern Filiz :: VoiceId
pattern Filiz = VoiceId' "Filiz"

pattern Geraint :: VoiceId
pattern Geraint = VoiceId' "Geraint"

pattern Giorgio :: VoiceId
pattern Giorgio = VoiceId' "Giorgio"

pattern Gwyneth :: VoiceId
pattern Gwyneth = VoiceId' "Gwyneth"

pattern Hans :: VoiceId
pattern Hans = VoiceId' "Hans"

pattern Ines :: VoiceId
pattern Ines = VoiceId' "Ines"

pattern Ivy :: VoiceId
pattern Ivy = VoiceId' "Ivy"

pattern Jacek :: VoiceId
pattern Jacek = VoiceId' "Jacek"

pattern Jan :: VoiceId
pattern Jan = VoiceId' "Jan"

pattern Joanna :: VoiceId
pattern Joanna = VoiceId' "Joanna"

pattern Joey :: VoiceId
pattern Joey = VoiceId' "Joey"

pattern Justin :: VoiceId
pattern Justin = VoiceId' "Justin"

pattern Karl :: VoiceId
pattern Karl = VoiceId' "Karl"

pattern Kendra :: VoiceId
pattern Kendra = VoiceId' "Kendra"

pattern Kimberly :: VoiceId
pattern Kimberly = VoiceId' "Kimberly"

pattern Lea :: VoiceId
pattern Lea = VoiceId' "Lea"

pattern Liv :: VoiceId
pattern Liv = VoiceId' "Liv"

pattern Lotte :: VoiceId
pattern Lotte = VoiceId' "Lotte"

pattern Lucia :: VoiceId
pattern Lucia = VoiceId' "Lucia"

pattern Lupe :: VoiceId
pattern Lupe = VoiceId' "Lupe"

pattern Mads :: VoiceId
pattern Mads = VoiceId' "Mads"

pattern Maja :: VoiceId
pattern Maja = VoiceId' "Maja"

pattern Marlene :: VoiceId
pattern Marlene = VoiceId' "Marlene"

pattern Mathieu :: VoiceId
pattern Mathieu = VoiceId' "Mathieu"

pattern Matthew :: VoiceId
pattern Matthew = VoiceId' "Matthew"

pattern Maxim :: VoiceId
pattern Maxim = VoiceId' "Maxim"

pattern Mia :: VoiceId
pattern Mia = VoiceId' "Mia"

pattern Miguel :: VoiceId
pattern Miguel = VoiceId' "Miguel"

pattern Mizuki :: VoiceId
pattern Mizuki = VoiceId' "Mizuki"

pattern Naja :: VoiceId
pattern Naja = VoiceId' "Naja"

pattern Nicole :: VoiceId
pattern Nicole = VoiceId' "Nicole"

pattern Penelope :: VoiceId
pattern Penelope = VoiceId' "Penelope"

pattern Raveena :: VoiceId
pattern Raveena = VoiceId' "Raveena"

pattern Ricardo :: VoiceId
pattern Ricardo = VoiceId' "Ricardo"

pattern Ruben :: VoiceId
pattern Ruben = VoiceId' "Ruben"

pattern Russell :: VoiceId
pattern Russell = VoiceId' "Russell"

pattern Salli :: VoiceId
pattern Salli = VoiceId' "Salli"

pattern Seoyeon :: VoiceId
pattern Seoyeon = VoiceId' "Seoyeon"

pattern Takumi :: VoiceId
pattern Takumi = VoiceId' "Takumi"

pattern Tatyana :: VoiceId
pattern Tatyana = VoiceId' "Tatyana"

pattern Vicki :: VoiceId
pattern Vicki = VoiceId' "Vicki"

pattern Vitoria :: VoiceId
pattern Vitoria = VoiceId' "Vitoria"

pattern Zeina :: VoiceId
pattern Zeina = VoiceId' "Zeina"

pattern Zhiyu :: VoiceId
pattern Zhiyu = VoiceId' "Zhiyu"

{-# COMPLETE
  Aditi,
  Amy,
  Astrid,
  Bianca,
  Brian,
  Camila,
  Carla,
  Carmen,
  Celine,
  Chantal,
  Conchita,
  Cristiano,
  Dora,
  Emma,
  Enrique,
  Ewa,
  Filiz,
  Geraint,
  Giorgio,
  Gwyneth,
  Hans,
  Ines,
  Ivy,
  Jacek,
  Jan,
  Joanna,
  Joey,
  Justin,
  Karl,
  Kendra,
  Kimberly,
  Lea,
  Liv,
  Lotte,
  Lucia,
  Lupe,
  Mads,
  Maja,
  Marlene,
  Mathieu,
  Matthew,
  Maxim,
  Mia,
  Miguel,
  Mizuki,
  Naja,
  Nicole,
  Penelope,
  Raveena,
  Ricardo,
  Ruben,
  Russell,
  Salli,
  Seoyeon,
  Takumi,
  Tatyana,
  Vicki,
  Vitoria,
  Zeina,
  Zhiyu,
  VoiceId' #-}

instance FromText VoiceId where
    parser = (VoiceId' . mk) <$> takeText

instance ToText VoiceId where
    toText (VoiceId' ci) = original ci

-- | Represents an enum of /known/ $VoiceId.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VoiceId where
    toEnum i = case i of
        0 -> Aditi
        1 -> Amy
        2 -> Astrid
        3 -> Bianca
        4 -> Brian
        5 -> Camila
        6 -> Carla
        7 -> Carmen
        8 -> Celine
        9 -> Chantal
        10 -> Conchita
        11 -> Cristiano
        12 -> Dora
        13 -> Emma
        14 -> Enrique
        15 -> Ewa
        16 -> Filiz
        17 -> Geraint
        18 -> Giorgio
        19 -> Gwyneth
        20 -> Hans
        21 -> Ines
        22 -> Ivy
        23 -> Jacek
        24 -> Jan
        25 -> Joanna
        26 -> Joey
        27 -> Justin
        28 -> Karl
        29 -> Kendra
        30 -> Kimberly
        31 -> Lea
        32 -> Liv
        33 -> Lotte
        34 -> Lucia
        35 -> Lupe
        36 -> Mads
        37 -> Maja
        38 -> Marlene
        39 -> Mathieu
        40 -> Matthew
        41 -> Maxim
        42 -> Mia
        43 -> Miguel
        44 -> Mizuki
        45 -> Naja
        46 -> Nicole
        47 -> Penelope
        48 -> Raveena
        49 -> Ricardo
        50 -> Ruben
        51 -> Russell
        52 -> Salli
        53 -> Seoyeon
        54 -> Takumi
        55 -> Tatyana
        56 -> Vicki
        57 -> Vitoria
        58 -> Zeina
        59 -> Zhiyu
        _ -> (error . showText) $ "Unknown index for VoiceId: " <> toText i
    fromEnum x = case x of
        Aditi -> 0
        Amy -> 1
        Astrid -> 2
        Bianca -> 3
        Brian -> 4
        Camila -> 5
        Carla -> 6
        Carmen -> 7
        Celine -> 8
        Chantal -> 9
        Conchita -> 10
        Cristiano -> 11
        Dora -> 12
        Emma -> 13
        Enrique -> 14
        Ewa -> 15
        Filiz -> 16
        Geraint -> 17
        Giorgio -> 18
        Gwyneth -> 19
        Hans -> 20
        Ines -> 21
        Ivy -> 22
        Jacek -> 23
        Jan -> 24
        Joanna -> 25
        Joey -> 26
        Justin -> 27
        Karl -> 28
        Kendra -> 29
        Kimberly -> 30
        Lea -> 31
        Liv -> 32
        Lotte -> 33
        Lucia -> 34
        Lupe -> 35
        Mads -> 36
        Maja -> 37
        Marlene -> 38
        Mathieu -> 39
        Matthew -> 40
        Maxim -> 41
        Mia -> 42
        Miguel -> 43
        Mizuki -> 44
        Naja -> 45
        Nicole -> 46
        Penelope -> 47
        Raveena -> 48
        Ricardo -> 49
        Ruben -> 50
        Russell -> 51
        Salli -> 52
        Seoyeon -> 53
        Takumi -> 54
        Tatyana -> 55
        Vicki -> 56
        Vitoria -> 57
        Zeina -> 58
        Zhiyu -> 59
        VoiceId' name -> (error . showText) $ "Unknown VoiceId: " <> original name

-- | Represents the bounds of /known/ $VoiceId.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VoiceId where
    minBound = Aditi
    maxBound = Zhiyu

instance Hashable     VoiceId
instance NFData       VoiceId
instance ToByteString VoiceId
instance ToQuery      VoiceId
instance ToHeader     VoiceId

instance ToJSON VoiceId where
    toJSON = toJSONText

instance FromJSON VoiceId where
    parseJSON = parseJSONText "VoiceId"
