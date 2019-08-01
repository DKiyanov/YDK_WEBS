*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.07.2019 at 19:28:14
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: YDK_WEBS_ACT....................................*
DATA:  BEGIN OF STATUS_YDK_WEBS_ACT                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YDK_WEBS_ACT                  .
CONTROLS: TCTRL_YDK_WEBS_ACT
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *YDK_WEBS_ACT                  .
TABLES: YDK_WEBS_ACT                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
