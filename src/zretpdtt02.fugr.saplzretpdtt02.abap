*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE LZRETPDTT02TOP.                    " Global Data
  INCLUDE LZRETPDTT02UXX.                    " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE LZRETPDTT02F...                    " Subroutines
* INCLUDE LZRETPDTT02O...                    " PBO-Modules
* INCLUDE LZRETPDTT02I...                    " PAI-Modules
* INCLUDE LZRETPDTT02E...                    " Events
* INCLUDE LZRETPDTT02P...                    " Local class implement.
* INCLUDE LZRETPDTT02T99.                    " ABAP Unit tests

* DIESES INCLUDE NICHT MEHR AENDERN!                              *
* NEUE INCLUDES IN BESTEHENDE INCLUDES AUFNEHMEN!                 *

*------------------------------------------------------------------
*           PBO-Module
*------------------------------------------------------------------
* Spezielle Retail-Includes
*NCLUDE LMGD1OXX.     "zentrale PBO-Module Bildbausteine
  INCLUDE LMGD2OXX.     "zentrale PBO-Module Bildbausteine       Kopie
*wk/4.0 rejoined following includes
  INCLUDE LMGD1O01.                    "PBO-Module für Kurztexthandling
*nclude lmgd2o01.     "PBO-Module für Kurztexthandling         Kopie
*NCLUDE LMGD1O02.     "PBO-Module für Steuerhandling
  INCLUDE LMGD2O02.     "PBO-Module für Steuerhandling           Kopie
*wk/4.0 rejoined following includes
  INCLUDE LMGD1O03.                    "PBO-Module für Verbrauchswerte
*nclude lmgd2o07.     "PBO-Module für Verbrauchswerte Retail Kopie
  INCLUDE LMGD1O04.                    "PBO-Mdoule Mengeneinheiten
*NCLUDE LMGD1O05.     "PBO-Module für Prognosewerte
  INCLUDE LMGD2O06.     "PBO-Module für Prognosewerte Retail Kopie
  INCLUDE LMGD1O06.                    "PBO-Module für EAN
  INCLUDE LMGD1O07.                    "PBO-Module für Langtexte
  INCLUDE LMGD2O03.     "PBO-Module für Bon-/Etikettentexte spez. Retail
  INCLUDE LMGD2O05.     "PBO-Module für Plazierungsgruppen spez. Retail
  INCLUDE LMGD1O08.                    "PBO-Modules for Table controls
  include lmgd1O1J.                    "PBO Document subscreen
*------------------------------------------------------------------
*           PAI-Module
*------------------------------------------------------------------
  INCLUDE LMGD2I01.     "Prüfmodule Speziell für Retail      Ergänzung
INCLUDE LMGD2I02.     "Prüfmodule Speziell für Retail-Bon-/Etikettentext
INCLUDE LMGD2I03.     "Prüfmodule Speziell für Retail-Plazierungsgruppen
*INCLUDE LMGD1IXX.    "zentrale PAI-Module Bildbausteine
  INCLUDE LMGD2IXX.     "zentrale PAI-Module Bildbausteine    Kopie

INCLUDE LMGD1IYY.     "Gemeinsame PAI-Module Bildbaustein/Trägerprogramm
*mk/4.0 Kopie LMGD2I05 wieder mit Original LMGD1I01 vereint
 INCLUDE LMGD1I01.    "Prüfmodule Datenbilder  MARA, MAKT (Kopfbaustein)
*nclude lmgd2i05.     "Prüfmodule Datenbilder  MARA, MAKT (Kopfbaustein)
  INCLUDE LMGD1I02.     "Prüfmodule Datenbilder  MARC, MARD, MPGD
  INCLUDE LMGD1I03.     "Prüfmodule Datenbilder  QM-Daten (MARA/MARC)
  INCLUDE LMGD1I04.                    "Prüfmodule Datenbilder  MBEW
  INCLUDE LMGD1I05.                    "Prüfmodule Datenbilder  MFHM
  INCLUDE LMGD1I06.     "Prüfmodule Datenbilder  MLGN, MLGT
  INCLUDE LMGD1I07.                    "Prüfmodule Datenbilder  MPOP
  INCLUDE LMGD1I08.                    "Prüfmodule Datenbilder  MVKE
*wk/4.0 reunited following includes
  INCLUDE LMGD1I09.                    "Prüfmodule für Kurztexthandling
*nclude lmgd2i09.     "Prüfmodule für Kurztexthandling        Kopie
*NCLUDE LMGD1I10.     "PAI-Module für Steuerhandling
  INCLUDE LMGD2I10.     "PAI-Module für Steuerhandling           Kopie
*wk/4.0 rejoined following includes
  INCLUDE LMGD1I11.                    "PAI-Module für Verbrauchswerte
*nclude lmgd2i12.     "PAI-Module für Verbrauchswerte Retail Kopie
*NCLUDE LMGD1I13.     "PAI-Module für Prognosewerte
  INCLUDE LMGD2I11.     "PAI-Module für Prognosewerte Retail Kopie
  INCLUDE LMGD1I14.                    "PAI-Module EAN
  INCLUDE LMGD1I12.                    "PAI-Module Mengeneinheiten
  INCLUDE LMGD1I15.                    "PAI-Module für Langtexte
  INCLUDE LMGD1I17.                    "PAI-Module für TC-Steuerung
  include LMGD1I7Q.                    "PAI-Modules Document Subscreens
*
  INCLUDE LMGD1IHX.                    "Eingabehilfen Datenbilder
  INCLUDE LMGD2IHX.     "Eingabehilfen Datenbilder Retail   Ergänzung
*------------------------------------------------------------------
*
*           FORM-Routinen
*
*------------------------------------------------------------------
*NCLUDE LMGD1FXX.        "zentrale Formroutinen Bildbaustein
*NCLUDE LMGD1FYY.        "Gemeinsame Form-Routinen Bildbaustein/Tägerpr.
  INCLUDE LMGD1FSC.        "zentrale Blätterroutinen   Bildbausteine
  INCLUDE LMGD1F01.                    "Form-Routinen Kurztexthandling
*NCLUDE LMGD1F02.        "Form-Routinen Steuerhandling
  INCLUDE LMGD2F02.        "Form-Routinen Steuerhandling         Kopie
 INCLUDE LMGD1F03.        "Form-Routinen Verbrauchswerte / Prognosewerte
  INCLUDE LMGD1F04.                    "Form-Routinen Mengeneinheiten
  INCLUDE LMGD1F05.                    "Form-Routinen EAN
INCLUDE LMGD1F06.        "Form-Routinen II Verbrauchswerte/Prognosewerte
 INCLUDE LMGD2F03.        "Form-Routinen Bon/Etikettentexte Retail Spez.
 INCLUDE LMGD2F04.        "Form-Routinen Plazierungsgruppen Retail Spez.
* AHE: 09.04.99 - A (4.6a)
 INCLUDE LMGD2F05.        "Form-Routinen Varianten-EAN-Pfl. aus SA Spez.
* AHE: 09.04.99 - E
 INCLUDE lmgd2f06.    "Incoterms        "note 2389622
  INCLUDE LMGD1FHX.       "spezielle Eingabehilfen Bildbausteine
 INCLUDE LMGD2FHX.       "spezielle Eingabehilfen Bildbausteine   Retail
INCLUDE LMGMMFHX.       "allg. Routinen Eingabehilfen  Bildbaust/Tägerpg
* Spezielle Retail-Includes
  INCLUDE LMGD2FXX.        "zentrale Formroutinen Bildbausteine    Kopie
  INCLUDE LMGD2FYY.        "Gemeinsame.Forms Bildbaust./Tägerpr.   Kopie

* Form-Routinen für Datenbeschaffung Bildbausteine
* # Industrie - Daten ( generiert )
  INCLUDE MMMGXGUW.        "Holen der Daten auf den Bildbaustein
  INCLUDE MMMGXSUW.        "Übergeben der Daten vom Bildbaustein
* # Retail - Daten
  INCLUDE MMMWXGUW.        "Holen der Daten auf den Bildbaustein
  INCLUDE MMMWXSUW.        "Übergeben der Daten vom Bildbaustein
* generierte Form-Routinen für Bildbausteine
  INCLUDE MMMGXRBD.        "Zus. Vorschlagshandling before  Dialog
  INCLUDE MMMGXRAD.        "Zus. Vorschlagshandling after   Dialog

  INCLUDE LMGD2O04.

  INCLUDE LMGD2I04.

  INCLUDE LMGD2O08.         " PBO - Module Varianten - EANs Retail
  INCLUDE LMGD2I13.         " PAI - Module Varianten - EANs Retail

* AHE: 04.03.99 - A (4.6a)
  INCLUDE LMGD2O09.         " PBO - Module Pflege VAR-EANs aus SA
  INCLUDE LMGD2I14.         " PAI - Module Pflege VAR-EANs aus SA
* AHE: 04.03.99 - E

* JB: 21.04.1999 - A (4.6a)
  include lmgd2o10.         " PBO-Feldauswahl für Subscreen Listung
* JB: 21.04.1999 - E

  INCLUDE lmgd2o11.    "Incoterms        "note 2389622

  INCLUDE LMGD2I15.  "jw/4.6B

* DIESES INCLUDE NICHT MEHR AENDERN!                                 *
* NEUE INCLUDES IN BESTEHENDE INCLUDES AUFNEHMEN!                    *

  INCLUDE lmgd2i16.    "Incoterms        "note 2389622

*&SPWizard: Include inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE LZRETPDTT02F01 .
INCLUDE LZRETPDTT02O01 .
INCLUDE LZRETPDTT02I01 .

INCLUDE lzretpdtt02o02.

INCLUDE lzretpdtt02f02.
