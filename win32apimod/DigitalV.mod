IMPLEMENTATION MODULE DigitalV;
<*/NOWARN:I*>

(* USER MUST PROVIDE DriverCallback
PROCEDURE SEND_DGVSIGNAL(dwFlags : DWORD;
                         dwCallback: DWORD;
                         hDriver : HANDLE;
                         wDeviceID : WORD;
                         dwUser : DWORD;
                         dwPos : DWORD) : WINT;
VAR
    temp        : DWORD;
BEGIN
    IF dwFlags BAND MCI_VCR_SIGNAL_POSITION <> 0 THEN
        temp := dwPos;
    ELSE
        temp := dwUser;
    END;

    RETURN DriverCallback(dwCallback,
                        DCB_WINDOW,
                        CAST(HANDLE, wDeviceID),
                        MM_MCISIGNAL,
                        hDriver,
                        temp);

END SEND_DGVSIGNAL;
*)
END DigitalV.
