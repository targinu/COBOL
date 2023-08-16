       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   V2051P2.
       AUTHOR.       GIOVANI TARGINO SERRA.
       DATE-WRITTEN. 09/06/2022.
      *--------------------------------------------------------------*
      * DISCIPLINA: PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *           CALCULAR A MEDIA ARITMETICA BIMESTRAL
      *           GRAVAR ARQUIVO FISICO SEQUENCIAL(WRITE)
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    FEV/2013 010002  SISTEMA GERA ARQUIVO SEQUENCIAL     *
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       FILE-CONTROL.
           SELECT P2CADV ASSIGN TO P2JOBV
                  FILE STATUS   IS FS-ERRO
           .
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       FD  P2CADV
           LABEL RECORD STANDARD
           RECORDING MODE  F
           .
       01  REG-P2CADV             PIC X(70)
           .
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-PORC                PIC 9(03)V99.
           05  WS-VALVENDA            PIC 9(04)V99.
           05  WS-FIM                 PIC X(01).
           05  WS-CTLIDO              PIC 9(02).
           05  WS-CTGRAV              PIC 9(02).
           05  FS-ERRO                PIC X(02).
           05  WS-MSG                 PIC X(30).
           05  WS-FS-MSG              PIC X(02).
      *    05  WS-MEDIA               PIC 9(02)V99.

      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-DEP              PIC X(02).
           05 WS-COD              PIC 9(03).
           05 WS-PROD             PIC X(15).
           05 WS-QTD              PIC 9(04).
           05 WS-UF               PIC X(02).
           05 WS-VALCOMP          PIC 9(03)V99.

      *-----> SAIDA - ARQ. FISICO SEQUENCIAL
       01  AS-REG-P2CADV.
           05 AS-DEP              PIC 9(04).
           05 AS-PROD             PIC X(20).
           05 AS-QTD              PIC X(01).
           05 AS-UF               PIC 9(02).
           05 AS-VALCOMP          PIC X(12).
           05 AS-PORC             PIC 9(02)V99.
           05 AS-VALVENDA         PIC 9(02)V99.
           05 AS-MEDIA-S          PIC 9(02)V99.
           05 FILLER              PIC X(19)       VALUE SPACES.

       01  WS-HIFEN               PIC X(80)       VALUE ALL '-'.

       01  FILLER                 PIC X(35)       VALUE SPACES.

      *-----> SAIDA - SYSOUT
       01 WS-REG-SYSOUT.
           05 DEPTO               PIC XX.
           05 FILLER              PIC X(1)       VALUE SPACES.
           05 COD                 PIC 999.
           05 FILLER              PIC X(1)       VALUE SPACES.
           05 PROD                PIC X(15).
           05 FILLER              PIC X(1)       VALUE SPACES.
           05 QTD                 PIC Z.ZZ9.
           05 FILLER              PIC X(1)       VALUE SPACES.
           05 UF                  PIC XX.
           05 FILLER              PIC X(1)       VALUE SPACES.
           05 VALC                PIC ZZ9,99.
           05 FILLER              PIC X(1)       VALUE SPACES.
           05 PORC                PIC ZZ9,99.
           05 FILLER              PIC X(1)       VALUE SPACES.
           05 VALV                PIC Z.ZZ9,99.

      *          '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-V2051P2.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY '================================================'
           DISPLAY 'GIOVANI TARGINO SERRA                           '
           DISPLAY 'ADS-VA5 - COBOL PARA MAINFRAME                  '
           DISPLAY '------------------------------------------------'
           DISPLAY 'CODIGO.PRODUTO........QTDE'
                                        '..UF.R$COMP.%LUCRO..R$VENDA'
           DISPLAY '------------------------------------------------'

           OPEN OUTPUT P2CADV
           IF FS-ERRO NOT = '00'
              MOVE  'ERRO AO ABRIR O P2CADV'  TO WS-MSG
              MOVE   FS-ERRO                  TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    LEITURA DADOS DA SYSIN
      *--------------------------------------------------------------*
       025-LER-SYSIN.

           ACCEPT WS-REG-SYSIN  FROM SYSIN

           IF WS-REG-SYSIN = ALL '9'
              MOVE   'S'     TO  WS-FIM
           ELSE
              ADD 1  TO WS-CTLIDO
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN ATE FIM DOS REGISTROS
      *--------------------------------------------------------------*
       030-PROCESSAR.

      *    COMPUTE AS-VALVENDA = (WS-NOTA1-IN + WS-NOTA2-IN) / 2
      *    MOVE WS-REG-SYSIN   TO   WS-REG-P2CADV
      *    MOVE WS-MEDIA       TO   WS-MEDIA-S
           WRITE REG-P2CADV   FROM  AS-REG-P2CADV
           IF  FS-ERRO NOT = '00'
               MOVE 'ERRO NA GRAVACAO DO CADALUN'  TO WS-MSG
               MOVE  FS-ERRO                    TO WS-FS-MSG
               GO TO 999-ERRO
           ELSE
               ADD 1 TO WS-CTGRAV
           END-IF

           MOVE WS-DEP      TO DEPTO
           MOVE WS-COD      TO COD
           MOVE WS-PROD     TO PROD
           MOVE WS-QTD      TO QTD
           MOVE WS-UF       TO UF
           MOVE WS-VALCOMP  TO VALC
           MOVE WS-PORC     TO PORC
           MOVE WS-VALVENDA TO VALV

           DISPLAY WS-REG-SYSOUT

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - V2051P2         *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS    - SYSIN  = ' WS-CTLIDO
           DISPLAY ' * REGISTROS GRAVADOS - CADALU = ' WS-CTGRAV
           DISPLAY ' *========================================*'

           CLOSE  P2CADV
           IF FS-ERRO NOT = '00'
              MOVE  'ERRO AO FECHAR O CADALU'  TO WS-MSG
              MOVE   FS-ERRO                TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF

           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO V2051P2         *'
           DISPLAY ' *----------------------------------------*'
           .
      *--------------------------------------------------------------*
      *    ROTINA DE ERRO
      *--------------------------------------------------------------*
       999-ERRO.

           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *           PROGRAMA CANCELADO           *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * MENSAGEM    = ' WS-MSG
           DISPLAY ' * FILE STATUS = ' WS-FS-MSG
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *       TERMINO ANORMAL DO V2051P2       *'
           DISPLAY ' *----------------------------------------*'
           STOP RUN
           .
      *---------------> FIM DO PROGRAMA XXPRG002 <-------------------*
