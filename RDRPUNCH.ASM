RDR      TITLE 'RDRPUNCH - TRANSFERS INPUT TO SYSPCH - VSE/ESA 2.3.0'   00000100
         PRINT ON,GEN                                                   00000200
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000300
*                                                                     * 00000400
* Dapsco Information Systems                          Written by:     * 00000500
* 3110 Kettering Boulevard                         Dave Leonard Clark * 00000600
* Dayton, OH  45439                                  (937)294-5331    * 00000700
*                                                                     * 00000800
* =================================================================== * 00000900
*                                                                     * 00001000
*                               PURPOSE                               * 00001100
*                                                                     * 00001200
*      This routine is  intended  for  use  in submitting jobs and/or * 00001300
* data from the "SYSIPT" system logical  unit (by default or with the * 00001400
* "CARD" option) or  from  sequential  disk  (with the "DISK" option) * 00001500
* through the VSE/POWER internal reader.   Up  to eight levels of JCL * 00001600
* are possible with each  execution  of  the RDRPUNCH program (taking * 00001700
* the default or using the "CARD" option) shifting each SYSIPT record * 00001800
* to the left by  one  (1)  column  (with  the exceptions noted under * 00001900
* "INVOCATION" below).  This routine also resolves any symbolic names * 00002000
* (when taking the default or using the "CARD" option) defined at the * 00002100
* current  execution   level   (with   the   exceptions  noted  under * 00002200
* "INVOCATION" below).                                                * 00002800
*      This function is most  useful  for  wrapping JCL around SYSPCH * 00002300
* data produced by some other  program (e.g.   LIBR or a compiler) or * 00002400
* disk data extracted by this  program (using the "DISK" option) that * 00002500
* needs to be processed  further  without  having to assign SYSIPT or * 00002600
* SYSPCH to a tape or disk file.                                      * 00002700
*                                                                     * 00002800
* =================================================================== * 00002900
*                                                                     * 00003000
*                           IMPLEMENTATION                            * 00003100
*                                                                     * 00003200
*      This routine executes as a standard batch program.   No system * 00003300
* changes are usually required to  implement it.   The one change you * 00003400
* may wish (or need) to make  is  to have your standard assignment of * 00003500
* the "SYSPCH" system logical unit be to a device defined at IPL time * 00003600
* as follows:                                                         * 00003700
*                                                                     * 00003800
*      ADD FED,2520B2      POWER DUMMY PUNCH,  DO NOT DELETE          * 00003900
*                                                                     * 00004000
* Although "FED" above may  generally  be  any site selected address, * 00004100
* this is the one VSE/ESA  supplies  and  does not have to correspond * 00004200
* with a real device.  The "2520B2" device is supposed to prevent the * 00004300
* occurrence of additional blank records appearing in punched output. * 00004400
*                                                                     * 00004500
* =================================================================== * 00004600
*                                                                     * 00004700
*                             INVOCATION                              * 00004800
*                                                                     * 00004900
*      Invocation is as with any  other standard batch program except * 00005000
* no other JCL is required (except  a  DLBL for your SEQIPT file when * 00005100
* using the "DISK" option) as long  as SYSIPT (for the "CARD" option) * 00005200
* and  SYSPCH  have  standard  permanent  assignments.    An  example * 00005300
* execution showing all three invocation methods is as follows:       * 00005400
*                                                                     * 00005500
* * $$ JOB JNM=MAINJOB,DISP=D,CLASS=0                                 * 00005600
* * $$ LST LST=FEE,DISP=D,CLASS=A                                     * 00005700
* * $$ PUN PUN=FED,DISP=I,CLASS=0                                     * 00005800
* // JOB    MAINJOB    JOB TO SUBMIT SUBJOB                           * 00005900
*     .                                                               * 00006000
*     .                                                               * 00006100
*     .                                                               * 00006200
* // EXEC   PGM=RDRPUNCH,SIZE=RDRPUNCH,PARM='CARD'                    * 00006300
* |* $$ LST LST=FEE,DISP=D,CLASS=A                                    * 00006400
* |// JOB    SUBJOB     JOB TO BE SUBMITTED BY MAINJOB                * 00006500
* |    .                                                              * 00006600
* |    .                                                              * 00006700
* |    .                                                              * 00006800
* |// EXEC   PGM= . . .                                               * 00006900
* /* EOD                                                              * 00007000
* // DLBL   SEQIPT,'your.dataset.name',,VSAM,DISP=(OLD,DELETE,KEEP)   * 00007100
* // EXEC   PGM=RDRPUNCH,SIZE=RDRPUNCH,PARM='DISK'                    * 00007200
* /* EOD                                                              * 00007300
* // EXEC   PGM=RDRPUNCH,SIZE=RDRPUNCH      <--- defaults to 'CARD'   * 00007400
* |/* EOD                                                             * 00007500
* |/& EOJ                                                             * 00007600
* /* EOD                                                              * 00007700
*     .                                                               * 00007800
*     .                                                               * 00007900
*     .                                                               * 00008000
* /& EOJ                                                              * 00008100
* * $$ EOJ                                                            * 00008200
*                                                                     * 00008300
* Note that the class specified on the  "* $$ PUN DISP=I" line is the * 00008400
* desired execution class for the subsequent internal reader job.     * 00008500
*                                                                     * 00008600
*                Notes on the 'CARD' option input data                * 00008700
*                                                                     * 00008800
*      The content of column one (1) on the SYSIPT data input to this * 00008900
* routine is immaterial  and  may  even be a space.   However,  lines * 00009000
* beginning with a space  or  an asterisk are not shifted;  but,  are * 00009100
* merely passed through as-is.   Also, if you do not wish to have the * 00009200
* symbolic names in a given  line  resolved,  you may place an EBCDIC * 00009300
* "NOT" symbol ('¬' or '^')  in  column one (1) to prevent resolution * 00009400
* of that line or place an  EBCDIC "TILDA" symbol ('~') in column two * 00009500
* (2) to prevent  resolution  only  at  the  current execution level. * 00009600
* Obviously,  the use of column  two  implies  that that line must be * 00009700
* processed by this routine at the next execution level.              * 00009700
*                                                                     * 00009700
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00009800
         EJECT                                                          00009900
* =================================================================== * 00010000
COMREG   MAPCOMR EQU=YES               PARTITION COMMUNICATION REGION   00010100
         USING COMREG,R1               ESTABLISH REG 1 AS BASE          00010200
* =================================================================== * 00010300
         EJECT                                                          00010400
* =================================================================== * 00010500
RDRPARM  DSECT                         RDRPUNCH PARMS DUMMY SECTION     00010600
         USING *,R10                   ESTABLISH REG 10 AS BASE         00010700
* =================================================================== * 00010800
RDRLENG  DS    H                       LENGTH OF REQUEST                00010900
RDRREQU  DS    CL4                     REQUEST                          00011000
* =================================================================== * 00011100
         EJECT
* =================================================================== *
DYNASTOR DSECT
* =================================================================== *
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA
*                                                                       00025700
DBLWORD  DS    D                       DOUBLE-WORD WORK AREA            00028000
RETCODE  DS    H                       RETURN CODE VALUE                00028100
WRKLENG  DS    H                       LENFLD                           00028200
WRKREGS  DS    15F                     REGISTER SAVEAREA                00028300
WRKOPTB  DS    XL1                     OPTION BYTE
WRKPARM  DS    CL7                     PARMNAME                         00028700
WRKVALU  DS    CL50                    VALBUF                           00028400
WRKALGN  DS    XL2                     (ALIGNMENT)                      00028500
WRKAREA  DS    XL100                   AREA                             00028600
*
DYNALENG EQU   *-DYNASTOR              LENGTH OF DYNAMIC STORAGE
* =================================================================== *
         EJECT                                                          00011200
* =================================================================== * 00011300
*        REGISTER USAGES                                                00011400
* =================================================================== * 00011500
R0       EQU   0                       PARM REG                         00011600
R1       EQU   1                       PARM REG                         00011700
R2       EQU   2                                                        00011800
R3       EQU   3                                                        00011900
R4       EQU   4                                                        00012000
R5       EQU   5                                                        00012100
R6       EQU   6                                                        00012200
R7       EQU   7                                                        00012300
R8       EQU   8                                                        00012400
R9       EQU   9                                                        00012500
R10      EQU   10                                                       00012600
R11      EQU   11                                                       00012700
R12      EQU   12                      RDRPUNCH CSECT BASE REG          00012800
R13      EQU   13                      REGISTER SAVEAREA                00012900
R14      EQU   14                      RETURN REG                       00013000
R15      EQU   15                      ENTRY REG                        00013100
* =================================================================== * 00013200
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         COPY  HEADER
         COPY  IF
         COPY  TRAILER
         COPY  WHILE
* =================================================================== * 00012800
         EJECT                                                          00013300
* =================================================================== * 00013400
RDRPUNCH HEADER VERSION='1.3',AUTHOR='DLC',DATE='(C)AUG92',            X00013500
               BASE=R12,SVAREA=SALEN
* =================================================================== * 00013700
       USING   DYNASTOR,R13
       XC      RETCODE,RETCODE     ZERO RETURN CODE
       MVI     PCH,C'V'            SET PUNCH STACKER SELECT CODE
*                                                                       00014200
       LR      R10,R1              COPY ADDRESS OF PARM LIST            00013800
       IF      R10,(NE,CR),R12     IF A PARM LIST WAS PASSED            00013900
        L      R10,0(,R10)          GET ADDRESS OF PARAMETER            00014000
       ENDIF                                                            00014100
*                                                                       00014200
       IF      RDRREQU,(EQ,CLC),=C'DISK'                                00014300
        OPEN   SEQIPT,SYSPCH        OPEN FILES                          00014400
       ELSE                                                             00014500
        OPEN   SYSIPT,SYSPCH        OPEN FILES                          00014600
       ENDIF                                                            00014700
*                                                                       00014800
GETNXT IF      RDRREQU,(EQ,CLC),=C'DISK'                                00014900
        GET    SEQIPT               GET INPUT RECORD                    00015000
        MVC    PCH+1(80),0(R2)      MOVE INPUT RECORD TO OUTPUT AREA    00015100
       ELSE                        ELSE 'CARD'
        GET    SYSIPT               GET INPUT RECORD                    00015400
        IF     IPT,NE,C' '
        AND    IPT,NE,C'*'
         MVI   IPT+80,C' '          INITIALIZE LAST COLUMN FOR SHIFT    00015500
         MVC   PCH+1(80),IPT+1      SHIFT LEFT 1 COLUMN FOR OUTPUT      00015600
        ELSE
         MVC   PCH+1(80),IPT        MOVE CARD INPUT TO OUTPUT AREA      00015600
        ENDIF
*                                   TRANSLATION GATES
        IF     IPT,NE,C' '          IF CURRENT LEVEL NOT BLANK          00015800
        AND    IPT,NE,C'¬'          AND CURRENT LEVEL NOT 'NOT'-SIGN    00015800
        AND    IPT,NE,C'^'          AND CURRENT LEVEL NOT 'NOT'-SIGN    00015800
        AND    PCH+1,NE,C'~'        AND NEXT LEVEL NOT A TILDE CHAR     00015800
         LA    R3,PCH+1              SCAN FROM COLUMN 1                 00016000
         LA    R4,PCH+72                  THRU COLUMN 71                00016100
         WHILE R3,(LT,CR),R4         SCAN COMMENTS FOR SYMBOLICS        00016300
          WHILE R3,(LT,CR),R4,AND,0(R3),NE,C'&&'                        00016500
           LA  R3,1(,R3)               FIND THE START OF A SYMBOL       00016600
          WEND                                                          00016700
          LA   R3,1(,R3)              POINT TO CHARACTER AFTER '&'      00016800
          IF   R3,(NLT,CR),R4                                           00016900
           WEXIT                       NO ROOM FOR SYMBOL NAME          00017000
TRTCMNT  TRT   0(0,R3),TRTABLE     (SEE "EX" INSTRUCTIONS BELOW)        00027500
MVCSYMB  MVC   WRKPARM(0),0(R5)    (SEE "EX" INSTRUCTIONS BELOW)        00027600
MVCCMNT  MVC   0(0,R7),0(R3)       (SEE "EX" INSTRUCTIONS BELOW)        00027700
MVCVALU  MVC   0(0,R5),WRKVALU     (SEE "EX" INSTRUCTIONS BELOW)        00027800
          ENDIF                                                         00017100
          LR   R5,R3                  SAVE START OF SYMBOL NAME         00017300
          XR   R1,R1                  CLEAR ADDRESS REG FOR "TRT"       00017400
          LR   R6,R4                  CALCULATE "TRT" LENGTH            00017500
          SR   R6,R5                  COMMENT END - SYMBOL START        00017600
          BCTR R6,0                    LESS 1 FOR ZERO REFERENCE        00017700
          EX   R6,TRTCMNT             SEARCH FOR END OF SYMBOL NAME     00017800
          LR   R3,R1                  GET ADDRESS WHERE "TRT" ENDED     00017900
          LR   R6,R3                  POINT AFTER END OF SYMBOL NAME    00018000
          SR   R6,R5                  CALCULATE LENGTH OF SYMBOL NAME   00018100
          IF   R6,(PLUS,LTR),R6,AND,R6,(NH,CH),=H'7'  CHECK SYMBOL LEN  00018300
           MVC WRKPARM,=CL256' '       CLEAR PARMNAME FOR BLANK PADDING 00018400
           MVC WRKVALU,=CL256' '       CLEAR VALUEBUF FOR BLANK PADDING 00018500
           BCTR R6,0                   DECR. LENGTH FOR ZERO REFERENCE  00018600
           EX  R6,MVCSYMB              MOVE SYMBOL NAME INTO PARM AREA  00018700
           STM R0,R14,WRKREGS          SAVE REGISTERS                   00018800
           LA  R6,WRKREGS              PUT SAVEAREA ADDRESS IN SAFE REG 00018900
           GETSYMB AREA=WRKAREA,       RETRIEVE, FOR THIS              *00019000
               PARMNAM=WRKPARM,         SYMBOL NAME, IT'S              *00019100
               VALBUF=WRKVALU,           REPLACMENT VALUE AND          *00019200
               LENFLD=WRKLENG             ASSOCIATED LENGTH             00019300
           LM  R0,R14,0(R6)            RESTORE REGISTERS                00019400
           IF  R15,(NZERO,LTR),R15     IF GETSYMB ERROR                 00019500
            LA R15,2                    SET RETURN CODE                 00019600
            STH R15,RETCODE             AND SAVE IT                     00019700
           ELSE                        ELSE                             00019800
            MVC WRKALGN(20),=CL256' '   CLEAR AREA AFTER SYMBOL VALUE   00019900
            LA R7,WRKVALU               GET ADDRESS OF SYMBOL VALUE     00020000
            AH R7,WRKLENG               CALC. WHERE TO PUT REM. COMMENT 00020100
            IF 0(R3),EQ,C'.'            IF REM. COMMENT STARTS WITH '.' 00020200
             LA R3,1(,R3)                TREAT IT AS PART OF SYMB. NAME 00020300
            ENDIF                       ENDIF                           00020400
            LR R8,R4                    GET ADDRESS OF END OF COMMENT   00020500
            SR R8,R3                    LENGTH OF REMAINING COMMENT     00020600
            BCTR R8,0                   DECR. FOR ZERO REFERENCE        00020700
            EX R8,MVCCMNT               CONCAT. REM. COMMENT & SYMB VAL 00020800
            BCTR R5,0                   POINT TO "&" OF SYMBOL NAME     00020900
            LA R8,1(,R8)                INCR. CMNT LENGTH FROM ZERO REF 00021000
            AH R8,WRKLENG               PLUS LENGTH OF SYMBOL VALUE     00021100
            LA R6,2(,R6)                LENGTH OF SYMBOLIC TO REPLACE   00021200
            IF R6,(PLUS,SH),WRKLENG     IF L'VALUE < L'SYMBOLIC         00021300
             AR R8,R6                    ADD SOME PADDING               00021400
             SR R3,R6                    AND ADJUST FOR NEXT SCAN LOC.  00021500
            ENDIF                       ENDIF                           00021600
            LR R6,R4                    GET ADDRESS OF END OF COMMENT   00021700
            SR R6,R5                    LENGTH OF COMMENT AVAILABLE     00021800
            IF R8,(LT,CR),R6            IF L'NEEDED < L'AVAILABLE       00021900
             LR R6,R8                    USE ONLY L'NEEDED              00022000
            ENDIF                       ENDIF                           00022100
            BCTR R6,0                   DECR. LENGTH FOR ZERO REF.      00022200
            EX R6,MVCVALU               MOVE SYMBOL VALUE INTO COMMENT  00022300
           ENDIF                       ENDIF GETSYMB ERROR              00022400
          ENDIF                       CHECK SYMBOL LENGTH               00022500
         WEND                        SCAN THRU COLUMN 80                00022700
        ENDIF                       TRANSLATION GATES                   00022900
       ENDIF                       DISK/CARD                            00015300
       PUT     SYSPCH                                                   00023100
       B       GETNXT                                                   00023200
*                                                                       00023300
FINISH EQU     *                                                        00023400
       IF      RDRREQU,(EQ,CLC),=C'DISK'                                00023500
        CLOSE  SYSPCH,SEQIPT        CLOSE FILES                         00023600
       ELSE                                                             00023700
        CLOSE  SYSPCH,SYSIPT        CLOSE FILES                         00023800
       ENDIF                                                            00023900
*                                                                       00024000
       MVC     IPT(60),=CL60'PGM ........  SET RETURN CODE=0000'        00024100
       LH      R15,RETCODE             RETRIEVE LAST RETURN CODE        00024200
       CVD     R15,DBLWORD             CHANGE $RC TO PACKED DECIMAL     00024300
       UNPK    IPT+30(4),DBLWORD+5(3)  UNPACK $RC INTO MESSAGE          00024400
       OI      IPT+33,X'F0'            AND CLEAR THE SIGN               00024500
       COMRG   REG=(R1)                GET ADDRESS OF COMREG IN R1      00024600
       MVC     IPT+4(8),IJBPHNAM       PUT EXEC PHASE NAME IN MESSAGE   00024700
       PUT     SYSLOG                  WRITE MESSAGE TO CONSOLE         00024800
*                                                                       00024900
       TRAILER RETCODE=RETCODE,SVAREA=SALEN                             00025000
* =================================================================== * 00025100
         EJECT                                                          00025200
* =================================================================== * 00025300
STORAGE  DS    0D                                                       00025400
* =================================================================== * 00025500
TRTABLE  DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  0.                  00025800
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  1.                  00025900
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  2.                  00026000
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  3.                  00026100
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  4.                  00026200
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF'  5.                  00026300
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  6.                  00026400
         DC    X'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF'  7.                  00026500
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  8.                  00026600
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  9.                  00026700
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  A.                  00026800
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'  B.                  00026900
         DC    X'FF000000000000000000FFFFFFFFFFFF'  C.                  00027000
         DC    X'FF000000000000000000FFFFFFFFFFFF'  D.                  00027100
         DC    X'FFFF0000000000000000FFFFFFFFFFFF'  E.                  00027200
         DC    X'00000000000000000000FFFFFFFFFFFF'  F.                  00027300
*                .0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F                       00027400
SALEN    DC    A(DYNALENG)
*
IPT      DC    CL81' '                 INPUT MESSAGE AREA               00028900
PCH      DC    CL81' '                 OUTPUT MESSAGE AREA              00029000
*                                                                       00029100
         LTORG                                                          00029200
* =================================================================== * 00029300
         EJECT                                                          00029400
* =================================================================== * 00029500
SEQIPT   DTFSD RECFORM=FIXBLK,BLKSIZE=80,RECSIZE=80,EOFADDR=FINISH,    *00029600
               IOREG=(2)                                                00029700
* =================================================================== * 00029800
         EJECT                                                          00029900
* =================================================================== * 00030000
SYSIPT   DTFDI DEVADDR=SYSIPT,IOAREA1=IPT,RECSIZE=81,EOFADDR=FINISH     00030100
* =================================================================== * 00030200
         EJECT                                                          00030300
* =================================================================== * 00030400
SYSPCH   DTFDI DEVADDR=SYSPCH,IOAREA1=PCH,RECSIZE=81                    00030500
* =================================================================== * 00030600
         EJECT                                                          00030700
* =================================================================== * 00030800
SYSLOG   DTFCN DEVADDR=SYSLOG,IOAREA1=IPT,BLKSIZE=60                    00030900
* =================================================================== * 00031000
         END                                                            00031100
