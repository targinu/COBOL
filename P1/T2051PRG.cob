       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   T2051PRG.
       AUTHOR.       GIOVANI TARGINO SERRA.
       DATE-WRITTEN. 07/04/2022.
      *--------------------------------------------------------------*
      * DISCIPLINA PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *           CALCULAR A MEDIA ARITMETICA BIMESTRAL
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    FEV/2013 010001  SISTEMA MOSTRA SYSOUT
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
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05 WS-FIM                 PIC X(01).
           05 WS-CTLIDO              PIC 9(02).
           05 WS-MEDIA               PIC 9(02)V99.
           05 WS-DATA                PIC 9(08).
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-CIDADE           PIC 9(04).
           05 WS-ESTADO           PIC X(2).
           05 WS-QTD-VEICULOS     PIC 9(07).
           05 WS-BAFOMETRO        PIC X(01).
           05 WS-QTD-ACIDENTES    PIC 9(04).
           05 WS-QTD-OBITOS       PIC 9(04).

       01  WS-REG-SYSOUT.
           05 WS-CID              PIC 9(04).
           05 FILLER              PIC X(1) VALUE '/'.
           05 WS-UF               PIC X(2).
           05 FILLER              PIC X(1) VALUE SPACES.
           05 WS-VEICS            PIC Z.ZZZ.ZZ9.
           05 FILLER              PIC X(1) VALUE SPACES.
           05 WS-BAFO             PIC X(1).
           05 FILLER              PIC X(1) VALUE SPACES.
           05 WS-ACIDS            PIC Z.ZZ9.
           05 FILLER              PIC X(1) VALUE SPACES.
           05 WS-OBITOS           PIC Z.ZZ9.
           05 FILLER              PIC X(1) VALUE SPACES.
           05 WS-PORC-ACIDS       PIC ZZ9,99.
           05 FILLER              PIC X(1) VALUE '%'.
       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-T2051PRG.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.
           DISPLAY "GIOVANI TARGINO SERRA"
           DISPLAY "1680482012051"
           ACCEPT WS-DATA FROM DATE
           DISPLAY "2" WS-DATA(2:3)"/" WS-DATA(5:2)" - FATEC"
           DISPLAY "---------------------"
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

           DISPLAY WS-PORC-ACIDS

           MOVE WS-CIDADE          TO WS-CID
           MOVE WS-ESTADO          TO WS-UF
           MOVE WS-QTD-VEICULOS    TO WS-VEICS
           MOVE WS-BAFOMETRO       TO WS-BAFO
           MOVE WS-QTD-ACIDENTES   TO WS-ACIDS
           MOVE WS-QTD-OBITOS      TO WS-OBITOS

           DISPLAY WS-REG-SYSOUT

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY '----------------------------------------'
           DISPLAY 'CIDADE COM MAIOR QTD. DE ACIDENTES:     '
           DISPLAY 'QTD. DE ACIDENTES DA CIDADE ACIMA.:     '
           DISPLAY 'QTD. DE CIDADES PESQUISADAS.......:     '
           DISPLAY '----------------------------------------'
           .
      *---------------> FIM DO PROGRAMA RSPRG002 <-------------------*
