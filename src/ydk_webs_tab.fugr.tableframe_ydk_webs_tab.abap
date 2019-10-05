*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_YDK_WEBS_TAB
*   generation date: 05.10.2019 at 20:33:55
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_YDK_WEBS_TAB       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
