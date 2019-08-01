FUNCTION ydk_webs_save_log.
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(LOG) TYPE  YDK_WEBS_LOG
*"----------------------------------------------------------------------

  DATA: xlog TYPE ydk_webs_log.

  DO 1000 TIMES.
    log-num = sy-index.

    SELECT SINGLE FOR UPDATE * INTO xlog
      FROM ydk_webs_log
     WHERE wsusr = log-wsusr
       AND cpudt = log-cpudt
       AND cputm = log-cputm
       AND num   = log-num.
    IF xlog-action IS INITIAL.
      INSERT ydk_webs_log FROM log.
      RETURN.
    ENDIF.
  ENDDO.
ENDFUNCTION.
