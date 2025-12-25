IMPLEMENTATION MODULE CharClass;


(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman

          Implementation © 1993
                by R. Sutcliffe
       (Portions coded by G. Tischer)
        Trinity Western University
7600 Glover Rd., Langley, BC Canada V3A 6H4
         e-mail: rsutc@twu.ca

    Stony Brook compiler port Implementation ported
    from above implementation source, those
        portions copyright (c) 1994-2004
        by ADW Software
    procedures are marked as modified
=========================================== *)

%IF Windows %THEN

FROM WINUSER IMPORT
    IsCharUpper, IsCharLower, IsCharAlpha;

%END

  (* Classification of values of the type CHAR *)

PROCEDURE IsNumeric(ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch is classified as a numeric character *)
BEGIN
    RETURN (ch >= '0') AND (ch <= '9');
END IsNumeric;

PROCEDURE IsLetter(ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch is classified as a letter *)
  (* modified by Stony Brook *)
BEGIN
    %IF Windows %THEN
        RETURN IsCharAlpha(ch);
    %ELSE
        RETURN ((ch >= 'A') AND (ch <= 'Z')) OR ((ch >= 'a') AND (ch <= 'z'));
    %END
END IsLetter;

PROCEDURE IsUpper(ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch is classified as an upper case letter *)
  (* modified by Stony Brook *)
BEGIN
    %IF Windows %THEN
        RETURN IsCharUpper(ch);
    %ELSE
        RETURN (ch >= 'A') AND (ch <= 'Z');
    %END
END IsUpper;

PROCEDURE IsLower (ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch is classified as a lower case letter *)
  (* modified by Stony Brook *)
BEGIN
    %IF Windows %THEN
        RETURN IsCharLower(ch);
    %ELSE
        RETURN (ch >= 'a') AND (ch <= 'z');
    %END
END IsLower;

PROCEDURE IsControl(ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch represents a control function *)
BEGIN
    RETURN (ch > '') AND (ch < ' ');
END IsControl;

PROCEDURE IsWhiteSpace(ch : CHAR) : BOOLEAN;
  (* Returns TRUE if and only if ch represents a space character or a format
effector *)
BEGIN
    RETURN (ch = ' ') OR
           (ch = CHR(9)) OR     (* Horiz tab *)
           (ch = CHR(13)) OR    (* Carriage return *)
           (ch = CHR(10)) OR    (* line feed *)
           (ch = CHR(11)) OR    (* vertical tab *)
           (ch = CHR(12));      (* form feed *)
END IsWhiteSpace;

END CharClass.
