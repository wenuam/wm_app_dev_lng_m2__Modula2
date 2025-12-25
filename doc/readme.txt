Please review this file for changes and/or additions from the version you are currently using. This file, readme.txt, is also installed in the 'doc' subdirectory of the installation.

Documentation is online via the help file(s). If you are new to Stony Brook then start with the environment help file "Getting Started" help topic and go from there.

Revision History
*************** Build 1.5.87, December 16 2011 ***************
You can use the resource file VisualStyles.res (from M2 library folder) in order
to get more up to date look of you program:
<*/RESOURCE:VisualStyles*>

*************** Build 58, ??? 2007 ***************
DTS 7155 fixed.

Windows X64/AMD64 implemented.

The Windows subsystem version number has been removed as an environment option. Apparently at some point MS started using this with regards to CPUs. 4.0 = 386, 5.01 – Itanium, 5.02 = AMD64. The linker still supports this via command line (or response file) if necessary.

The Windows linker section alignment option has been removed. The linker still supports this via command line (or response file) if necessary. The MS linker does not like this option when not used with device driver linker options.

Debugger bugfix for viewing global variable of other modules using the qualified syntax. i.e. ModuleName.SymbolName

*************** Build 57, March 29 2007 ***************

Increased the exception message string storage from 64 to 256 characters. This requires a recompile of all sources with exception handlers. Changed the environment project and compiler symbol file version numbers to facilitate this.

WIN32SYSTEM procedure attribute and /CALLS directive items have been removed and replaced with the Windows procedure attribute and WindowsCall /CALLS directive. The new items work for 32-bit or 64-bit modes.

Bugfix for the peephole optimizer on IA32 and floating point code with LONGREAL types. Probably very rare as the bug has been in the source for at least 9 years.

Changed the Windows IA32 exception handling to use a signature magic number to identify M2 exception frames from frames of other langauges. The requires a recompile of everything. The SYM and project version numbers are changed to facilitate/require this.

*************** Build 56, ? 2007 ***************
Fix DTS 6478, 6472, 6556, 6506, 6594, 6609, 6622.

Fixed a bug with structured return types on SPARC processors.

Removed forced stack for structured return types on AMD64.

PMD support for AMD64.

Added a new procedure to the PMD output support. OutputUserPMD. This procedure imported from the SYSTEM module will allow you to generate a PMD from any given location is the program. You still use OutputCallTrace inside of exception handlers to output a PMD for the context of a handled exception.

Fixed a memory leak in PMD file output.

*************** Build 55, Febuary 23 2007 ***************
Bugfix for an IA32 compiler bug introduced in B54 for VALUE record parameter types. A compiler internal error occurred.

DIV and MOD for INTEGER types are now implemented as / and REM. Nobody cares about the Modulus operation which affects DIV and processors implement / and REM.

The COMPLEX data types have been eliminated. Nobody cares about this and it is simpler to not support across our various processors. Most complications arise due to parameter passing requirements.

The EXTENDED type, an 80-bit real, has been eliminated. It was only supported on Intel 32-bit so we dropped it. AMD64 uses SSE2 for floating point and SSE2 does not implement the 80-bit real.

Bugfix for REM on 64-bit INTEGER types. Bug was introduced in a recent build as part of AMD64 adjustments to the Intel assembler. The bug was in the SHRD/SHLD instruction encoding.

DTS 6277 fixed. Rev 17873. SPARC compiler code generation bug with split parameter types and the stackDepth allocation.

AMD64, X86-64, support is now implemented for Linux.

Bugfix for sparc floating point divide by zero checking code.

*************** Build 54, Jan 3 2006 ***************
Bugfix for environment. It was not updated when the compiler SYM file header format changed and the Boolean it patched into SYM files for modules in DLL projects was at the wrong location.

Changed all programs using the SYM file header info to use a common record type definition.

Bugfix for 64-bit ELF fixup output. It did not convert 64-bit unaligned offset fixups to the proper ELF output type in 64-bit mode.

Unused type cast warnings are now output under the unused code warning option.

Added a new option related to type cast warnings. This option makes type cast warnings compilation errors. This option is defaulted ON.

Bugfix for SPARC code generation. The bug was with parameter passing to a procedure which contained a nested procedure which had an uplevel reference to a parameter which was a stack parameter and the procedure also had a 64-bit integer parameter which was split across the last register parameter register and stack space. Upleve stack variables would be accessed with an offset 4-bytes too low due to the 64-bit split parameter.

Bugfix for debugger Local variables window on SPARC. The above mentioned compiler bug affected this code.

Bugfix for debugger when a DLL was unloaded and/or moved by the OS, then runtime checking breakpoints were not removed and if another DLL was loaded later at the address of the unloaded DLL then a breakpoint will exist in an unexpected location in the code.

*************** Build 53, 2006 ***************
Bugfix for attach process on Unix systems. The dialog resource was not properly defined. DTS 5358. Rev 14952

Bugfix for SPARC code generation parameter passing and 64-bit VAR parameter types when the first stack parameter was a VAR 64-bit type.

More overflow checking code will be output.

Added a new compiler directive. INITORDERBEFORE. See the environment help file for documentation. DTS 5280. Rev 15607.

SWAPENDIAN statements now support RECORD and ARRAY types. See the language reference for information. DTS 5456 Rev 15622.

64-bit Linux PPC supported.

*************** Build 52, August 24 2006 ***************
The linker and environment now default the AIX “max data” linker option to 2GB.

Bugfix for Linux/PPC debugger on systems where the NPTL threads library is used. Normally >=2.6 kernel systems. Apparently Linux uses different syscall numbers on different processors. On NPTL threads the debugger uses the _NR_tkill syscall to stop threads.

Bugfix for cross linking ELF (Linux, SunOS) from Little endian to big endian if variables were imported from shared objects. Programs using GTK would have such imports.

Simplified the librarian (sblib) command line. It was still mimmicing the old MS DOS librarian syntax. No need to still do this.

*************** Build 51, August 11 2006 ***************
Added a new feature to the compiler where it can give a warning when the size of the local variables allocated to a procedure are larger than some specified value. The value is specified in the environment project options dialog. Due to the option addition, and no unused padding available, the compiler SYM file version number has changed.

*************** Build 50, August 1 2006 ***************
Changed the linker to ignore symbols from shared objects for the “special” symbols for program entry, DLL entry and DLL termination. Some shared objects on Unix systems might be built badly and export their entry procedure(s). To avoid conflict with linking a shared object the linker will ignore those symbols from shared objects. The symbols are identified by the name given to the linker. The defaults are _start for program entry, _init for shared object initialization and _fini for shared object termination. This is a simple workaround and it also makes sense because it never makes sense that a program/dll has it’s init/term code be imported from some other dll.

Bugfix for the SPARC code generator in situations with
IF (LongType BAND Mask) =<> 0 THEN
A compiler internal error message was generated.

*************** Build 49, July 27 2006 ***************
Bugfix for AIX VAR stack parameter code generation.

Bugfix for AIX debugger “stop execution” command. This fix caused the addition of a feature to the debugger to mark threads as not viewable/selectable. Apparently in some situations AIX will not allow a debugger to read the thread resgister state. Specifically at some points while in kernel mode. This makes the fixed stop execution feature not quite so useful if many threads are not viewable at times.

*************** Build 48, July 6 2006 ***************
Bugfix for AIX PPC compiler on internal compiler helper procedures. The procedures were not correctly marked for the parameter stack space reserve. For example, and array slice help has four parameters. If no other code in the procecdure using the slice had at least four parameter then a problem would occur. If some other procedure with at least four parameters existed then no problem would exist.

*************** Build 47, July 5 2006 ***************
Bootstrapped the M2 system with checking enabled. This was done since the Saperion core code is built this way so the M2 system should be able to bootstrap that way as a basic functionality test, which is the whole point of a bootstrap in the first place. Building with checking will only be done as a test and not for release and general use. Numerous changes necessary in the compiler due to bit twiddling and checking issues. Some stupid hacks redone in a cleaner way. Some dynamically allocated arrays needed to be declared differently to function under checking. A handful of technical bugs actually found. As an example. Functionally some code was correct but a wrong variant field of the same type was being used. Correct in that the field being used was at the same offset as the correct field. This was in the Operand record.

Fixed a bug in PPC SYSTEMC.COPYOPENPARAM. Its use of the STACKGROWTH procedure was incorrect. STACKGROWTH is used when the open array parameter being copied is larger than a processor page size, 8192 for the PPC. The frame pointer register was clobbered by STACKGROWTH which is not correct for the COPYOPENPARAM procedure since it is called after the frame pointer register is setup in the procedure prolog.

Fixed a bug in the AIX debugger data read routine. It seems that in some circumstances the AIX debugger memory read API does not like unaligned byte reads. The AIX debugger now always starts reads on a 4-byte boundary.

Added a menu item to the debugger to force the debugger to reload a source file.

Added additional error handling code to the call trace window.  If the trace faults with an error a message will be down in the trace window so you know something happened. An error typically is a failure to read data from the application program. Unually due to a bogus address.

The Windows debugger ignores(auto propagates) any access violation with the module “offguard” in the call trace. Axel requested this.

*************** Build 46, June 15 2006 ***************
Added a loaded image info dialog to the debugger.

*************** Build 45, May 24 2006 ***************
Fixes for the debugging of Solaris system core dump files.

An addition to post mortem debugging (M2 or OS) to help when debugging on Windows a Unix executable. The feature is for finding the shared objects. The post mortem file gives full Unix paths which of course are meaningless on Windows.

Fixed a bug in the Linux post mortem debug output on Suse9 where the pmd file would seem to always be 2GB in size. Probably happens on all 2.6 kernel systems.

Added Saperion PMD file output to sbdump.

*************** Build 44, April 26 2006 ***************
Fixed a bug in SYSTEMEX.GetActiveFrameEx on the Solaris/SPARC platform. It caused a problem in the debugger faulted.

Added a parameter to SYSTEM.EnableCallTrace. The parameter allows to to specify a string that is added to the PMD file name. Typically you would use a version number string.

*************** Build 43, April 5 2006 ***************
Changed SYSTEMEX on IA32 (SEH) to not verify M2 exception frames. Causes issues with exception propagation across DLL boundaries when the DLLs are linked statically to the runtime library.

*************** Build 42, April 4 2006 ***************
Added the build number to the output of the debugger DbgDriver diagnostic output.

The error for imported modules that import a native source module is no longer reported during script execution unless the new environment option, show cross imports error, is enabled. The option is off by default.

The environment now tracks less information across DLL project boundaries. Could/Should help module name collisions.

The call trace facility in SYSTEMEX with a new post mortem debug facility. The procedure names the compiler uses are still EnableCallTrace and OutputCallTrace. This allows the current compilers to support the old SYSTEMEX modules should one desire to keep using an old SYSTEMEX rather than the new SYSTEMEX. The post mortem output file has a pmd file extension. The change requires code to be recompiled therefore we change the SYM and project file version numbers to make sure nothing old gets used.

Bugfix for debugger attach feature added with the recent changes for post mortem debugging. The ProgramEndian variable was not initialized. DTS 3728.

*************** Build 41, March 23 2006 ***************
Bugfix for SPARC debugger. The bug was in the procedure that read data from the debugee process. If the read returned less than the cached size request but equal to or larger than our read request there was a bug. The operating system apparently returns a lesser amount on reads on edge of an allocated memory region.

Bugfix for the clipboard usage on GTK+ 2.2 systems. Currently this in only Solaris/IA32 and AIX. Other systems are still limited to using GTK+ 2.0.

Additional debug driver diagnostic output in debuggers.

The debugger now restricts string output in the data/locals/watch windows to the Latin-1 character set. Any character outside this set will be output as the upside down question mark character.

BasicDialogs now supports a user error message box type. Previously it only supported App errors (internal errors) and warnings.

The error for imported modules that import a native source module no longer outputs a dialog in unattended builds (-unattended). The dialog now marks itself as an error.
 
*************** Build 40, March 6 2006 ***************
Bugfix to Unix debuggers introduced in Build 39. Bug reported, access violation, in the Linux debugger but theoretically possible in the SunOS debugger as well.

The SPARC code generation no output stack growth code. Previously we thought this was not necessary on SunOS/SPARC and it seems that it is necessary. All code source be recompiled.

The compiler no longer outputs DLL imported symbols in the globals section of debug information. The caused an access violation in the linker. No need for these symbols to be output.

Fixed an access violation in the linker due to debug fixups on imported symbols. The linker now gives a compilation error in this circumstance.

The environment now restores the main window on any message box displayed while running minimized. Minimized is commonly used during script execution.

*************** Build 39, Feburary 26 2006 ***************
Defect report 3172 resolved. The Windows debugger now places the process id number in the process attach dialog.

Bugfix for linker that could arise in a cross compile/link situation to operating systems like SunOS/SPARC and Linux/PPC from Windows/IA32. The OS barf on the resulting executable.

Bugfix for AIX debugger when searching for a symbolic procedure name. The leading ‘.’ Was always added the ‘.’ and it should not have been doing this.

Bugfix for the AIX debugger on runtime checking public symbol name resolution. The leading ‘.’ Was not added. This caused the debugger to set a breakpoint on the thunk data and not the actual procedure.

All Unix API interface files, like UNIX.def, should have the /DLLDEF directive in the source file. This is required for AIX and currently ignored on other systems.

*************** Build 38, Feburary 3 2006 ***************
Fix in the UNIX debuggers that could occur when using the “Stop execution” command (The red stoplight). Most likely to only occur on Linux but all debuggers had similar code changing the CurThread global. The setting of the CurThread global is now delayed after all threads are stopped (on Linux).

The build dialog should now be properly minimized when the environment is run minimized while executing a script. The bug was in the WinShell module.

Bugfix in PPC and SAPRC compiler code generation in the use of the ALTERS procedure attribute if you used ALTERS(). The code generation of the compiler assumes some registers are always altered. The parser now implicitly includes those registers.
 
Changes to UNIX.def and WIN*.def files incorporating differences in the M2 systems files and those in Saperion m2doslib.

Linux kernel 2.2 support has been removed from UNIX.def. 2.4 and later are supported.

On UNIX systems the signal handler that traps access violations, alignment violations and such could possibly crash is the exception occurred in code from the system or other language and their stack frame is not standard. The system now protects itself from exceptions that occur while walking the stack looking for exception handlers.

The Linux debugger now looks for WIFSIGNALLED when waiting for thread termination (WaitForThreadDeath).


*************** Build 37, January 27 2006 ***************

Build numbers are now XX.YY.ZZZZ. XX is the build number. YY is a bugfix for the build number. Used when some application is locked to a specific compiler build number. ZZZZ is the subversion revision number.

Removed the build number feature of the environment/linker/compiler. No longer used and does not work in a situation where multiple revisions are installed in the same account.

Fixed a bug in the Unix debuggers where the –install command line option did not function. This apparently got broken at some point because some messages in the command line parser use resources. The debugger now special cases a command line with only –install.

*************** Build 36, January 24 2006 ***************
Fixed a PPC compiler bug with procedure that accept a variable number of parameters (Variable procedure attribute).

Refined the Unix debugger filename case variation algorithm. Also the Unix debuggers now convert Windows absolute paths to relative paths. This should allow you to copy a Windows source directory structure to some directory on a Unix machine and enter only that base directory in the debugger source list. For example p:\something\mod\source.mod => something/mod/source.mod. On the Unix machine you can have a directory /pdrive and copy all p: source structure there and then the debugger will look for sources in /pdrive/…

Added the procedure EnableDialog to the DlgShell module.

The compiler now allows NIL as a value for dynamic arrays in constant declarations.

The environment now detects stray sym/obj files. Stray files are sym/obj files in the current project sym/obj directory where the file matches a module imported from another project.

Added a tweak to common subexpression optimization on fetches of indirect variables. These are now conditional common subexpressions if the variable is enregisterable.

Corrected a declaration in the Windows MMSYSTEM.def and SHLOBJ.def files.

Compiler bugfix for the dead code optimization. Dead assignments to parameters were not deleted. Register allocation using lifetime information expects these dead assignments to be done. This bug has always existed and only just showed up on the AIX PPC system.

Solaris on IA32 processors is now supported.

AIX now supported.

Fixed a bug in the Linux debugger for kernel 2.6 systems.

The peephole/assembly optimizer for SPARC code generation is implemented.

Added a new warning for the CAST function. The warning is generated for big endian targets when you cast from a larger to a smaller integer type.

The Unix debuggers now support attaching to an already running process.

The environment and debugger GUI on Unix systems now use GTK+ 2.x. Solaris 8 system will need to have this installed.

Added a warning to the environment in situations where an import module imports a module that is native to the current project.

The Unix debuggers can now propagate an access violation to the application code.

The debugger now allows you to choose what exceptions the debugger automatically stops execution on. This option is in the Options dialog. The option state is saved in the .sbd file.

The debugger now saves the “stop on external/unknown breakpoints” in the .sbd file.

The debugger can now do post mortem debugging of Linux, Solaris and AIX core files.

The debugger can do post mortem debugging of any operating system executable regardless of the host operating system.

Fixed a compiler bug where it did not detect an error in a procedure definition when one tried to put an anonymous pointer definition in a formal parameter list. PROCEDURE bug(param : POINTER TO CARDINAL);

Fixed a debugger bug on Window when terminating a program that had threads and one of the threads was stopped in the kernel of the operating system. The debugger would stop on an access violation on the ExitProcess procedure. The debugger tries to do a clean forcible termination by forcing execution of the ExitProcess procedure. It changed the ip address of threads to ExitProcess but you cannot do this if the thread is stopped in the kernel.

It is now a compilation error to pass an integer or floating point constant to a variable param procedure as a variable param unless the constant is specifically typed. i.e. ORD, INT, VAL and so on. In M2 constants are untyped until unsed in an expression. At that point they take on the type of the expression. A variable param has no type so the constant stays untyped. It is only untyped in a sense in that mostly like a LONGINT. Floating point constants are mostly like LONGREAL.

Fixed a compiler code generation bug on SPARC where and alignment violation occurred when passing a LONGREAL that was unaligned. The SPARC calling convention forces these unalignments and the code generation must handle it when it occurs.

*************** Build 35, Feb 15 2005 ***************
Fixed some problems on Unix systems with the change from “Saperion” for a config option directory to “Saperion M2”.

*************** Build 34, Feb 7 2005 ***************
Fixed linking of shared objects on PPC Linux.

Fixed the m2e/sbd config settings loading algorithm for Unix systems.

Fixed a bug in the PPC compiler when the CopyAttributes procedure attribute was used.

Fixed PPC compiler code generation problems with large stack allocations in a procedure.

Fixed a linker problem with DLL fixups on Linux PPC. The problem also existed in Solaris SPARC DLLs but the operating system seems to have an undocumented ability where the bad linker algorithm actually worked. The SPARC fixups were changed to the proper documented algorithm.

Fixed a debugger access violation related to a thread termination event.

The debugger now delays fetching thread registers until used. This is to enhance performance on the slow Linux PPC interface.

Fix a problem with the debugger source path where the debugger added paths to the list that already existed in the list.

Added a feature to the Unix debuggers where the debugger will try multiple filename case variations for debug source files. The name as listed in the debug info, the name all uppercase, the name all lowercase and the name as listed with an uppercase file extension. The feature is disabled by default. Use the options dialog to change the setting.

Fixed a bug in the debug “view both” display when scrolling then assembly listing when you had multiple DLLs with the same module.

Fixed compiler RISC code generation bug with FOR loops of types smaller than 32-bit. This was a parser optimization that is not valid unless the index variable becomes enregistered. This was a new (B33) tweak for SPARC compilers and obviously B33 was the first instance of the PPC compiler.

Fixed a PPC compiler bug with regards to procedures with more parameters than can fit in registers and the last parameter is a 8 or 16-bit type. This caused a misalignment and subsequent overlap of non register stack parameters and the display area (nested variable fp table).

For Unix systems the SIGXFSZ signal is now blocked so that API calls such as write, truncate will return an error, EFBIG, rather than terminate the application.

Fixed a compiler bug reported in the PPC compiler. This bug also exists in the SPARC compiler. The bug is with operators of long types (two register) where the low register of the result register pair is in the high register of one of the source operands. We did not take the easy way out a simply force a unique result when the result shares registers with one of the source operands. We specifically look for a cross register situation.

The environment and debugger config options are now saved under the directory “Saperion M2”. Previously the directory was “Saperion”. You will have to reset your customized options.

Fixed an environment bug when a compilation error occurred when the environment was executing under a script and the environment window was minimized. An unusual sequence of events transpired to cause a Windows message box call to not display itself and leave the main window disabled and therefore unresponsive to the mouse and keyboard.

*************** Build 33, Jan 5 2005 ***************
Conditional Common sub-expression optimization is now more efficient in some situations. This was found while examining code listings in the new POWER code generator. We saw some correct but stupid register allocation. Investigation determined it to be associated with conditional common subs associated with variables eventually enregistered. The adjustment has been put into all other code generators (IA32, SPARC).

The project version number has been changed. The RTL project naming convention has been changed. The default SYM and OBJ directory names have been changed. These changes have been made to allow the environment to support multiple processors for a given operating system. For example previously Linux was rtl32lnx and lnx32sym. No ability to have IA32 and PPC32 support without clobbering each others SYM/OBJ files. The new RTL naming convention is now rtl-os-processor. Os = (win, lnx, aix). Processor = (ia32, sparc32, ppc32). Therefore what was rtl32lnx is now rtl-lnx-ia32 or rtl-lnx-ppc32. The sym/obj naming convention has changed similarly. What was lnx32sym would now be lnxia32sym or lnxppc32sym. The same applies for object directories.

The m2rtl build batch, and m2lib.sbs environment script, have been updated to support multiple processors per OS. The Win32 parameter has been changed to Windows. If no processor is given to the batch then the default processor is built. The help file documents the use of m2rtl.

New script procedure, CreateProjectEx. This supports the above changes. The help file documents the procedure.

New script function. SystemIsEx. This supports the above changes. The help file documents the function.

The documentation for the script procedures CreateProject and SystemIs have been updated to reflect the multi-processor support.

User configuration settings are now saved in a Saperion options folder and no longer a “Stony Brook” folder. This means you will have to reset options saved in those folders. The default install directory for root is now /opt/Saperion/m2 and not “/opt/Stony Brook/m2”.

Unix systems now support installs to local accounts and the install script does not need to run while logged in as root. The default install directory for local accounts in $HOME/m2.

For those who cross compile on Windows for other Unix systems, the debugger can now be installed separately from the full m2 system. After unzipping the native debugger simply execute the debugger with the –install option. The directory where you unzip the debugger files should be the current directory when you use the –install option. The native debugger file names follow the convention of sbd_os_cpu. The following debuggers are supplied sbd_linux_ia32, sbd_linux_ppc32, sbd_sunos_sparc32, sbd_sunos_ia32, sbd_aix_ppc32.

Fixed a bug with the compiler SYSTEM.VASTART procedure. It did not properly mark a pseudo variable as addressed and therefore not enregisterable.

Fixed a bug in the SPARC code generation for some types of LONGINT or LONGCARD literal constants. The bug was found while testing the POWER compiler.

Fixed a bug in the SPARC code generation for a LONGINT multiply with overflow checking enabled. The bug was found while testing the POWER compiler.

Fixed a bug in the SPARC compiler with CASE statements of LONGINT/LONGCARD types. The case had to be a sparse case. The bug was found while testing the POWER compiler.

Fixed a bug in compiler code generation (all processors) for large set operations of the type a = a op b, where op can the the + - * / SYSTEM.SHIFT and SYSTEM.ROTATE. The bug has existed for over 10 years. The bug is extremely unlikely for IA32 code generation, which accounts for how the bug could exist for so long. SPARC has apparently been lucky. The bug was found in POWER compiler code generation.

Linux on PowerPC is now supported.

*************** Build 32, October 1 2004 ***************
The stripdebug utility program now handles .lib files that contain both code DLL imports and extended name only DLL imports.

The version tags dialog now cleans up returned text with forced line breaks, empty tags and duplicate names in the tags and/or valid tags list.

Fixed a bug in the environment script procedure RemoveVersionTag.

Added a new feature to the environment to aid in performing unattended builds. For information see the help file topic. Environment Reference, Command line format.

Environment builds that are running minimized should no longer show the build status dialog. The dialog was previously shown since Windows would not let me hide a modal dialog. The environment now uses a modeless dialog when minimized and fakes a modal loop for the script system.

*************** Build 31, September 20 2004 ***************
Fixed a compiler code generation bug with the SYSTEM.MOVE inline procedure when you passed a CARD16 or CARD8 to the count parameter.

Fixed a compiler bug with code generation alignment of CLASS implementation data (data defined in the implementation module of a class defined in the DEF).

Fixed a compiler code generation bug with module init code. It was possible for some module init code to not be called when the module circular dependency structure was somewhat severe. The init code sequence algorithm was scrapped and redone. We no longer try to smartly flatten the module dependency graph. The circular references situations keep causing problems. The new algorithm basically mimics the “classic” method of each module calling its imported modules initcode at runtime.

*************** Build 30, August 16 2004 ***************
Fixed a problem with the script option SetProjectOption(PrjObjFormat). It is never necessary to use this command.

The compiler bugfix in build 29 for module init code calling order had a problem with deep circular module dependencies. The compiler gave an internal error that it was not able to resolve the module dependency graph.

Fixed an access violation in the compiler when you tried to instance a generic module in a DEF module. A compilation error is the proper compiler response.

The compiler was requiring expression compatibility for the second parameter of the INCL/EXCL procedures. It should have only required assignment compatibility.

Added the BS_FLAT button style to the resource editor push/check/radio button styles dialogs.

The compiler did not allow a CLASS type reference in the MIN/MAX builtin functions. MAX(ClassType.ClassComponent)

Fixed a compiler condition compilation bug where the following gave a compilation error.
<*/VER:DBG*>
IF %NOT DBG %THEN
   <*/VER:IT*>
   %IF IT %THEN <= compiler says it is not a valid tag

Fixed a compiler bug where it did not give a compilation error when a FORWARD class was not implemented.

Fixed a compiler access violation caused by a syntax error when instanciating a generic module.

Fixed a bug in the strings module Insert procedure in a situation where you inserted a number of characters at a position that perfectly filled the destination error.

Fixed a compiler internal error on expressions that used CAST to typecast from a floating point expression to a set type.

The Linux debugger works with NPTL, kernel 2.6 systems.

Fixed a compiler internal error when passing the result of the CMPLX built-in function to a procedure parameter.

The 64-bit integer types now have an 8-byte natural alignment. This makes porting C header files using the C 64-bit integer types simpler. This also allow forward portability to 64-bit for disk based records.

Fixed a couple of problems building the Win32 RTL project using Unicode characters.

The environment should no longer hang doing builds on very fast dual processor machines.

The debugger can handle larger process lists when attempting to attach to a process.

A compiler problem with code generation on IA32 in procedures with very large amount of local stack space has been fixed.

The debugger now tries to stop in the last thread you were viewing when you use the stop execution command in the debugger. Previously it showed whatever thread the operating system the last debug event occurred in.

The debugger now shows the current thread execution location (line number and source file) in the View Menu select thread dialog. This should help determine which thread you want to select.

The debugger has changed a little in the use of the directories list. The directories in the list are now combined with the path information in the debug info first. This allows you use a directory in the list as a "root" directory. If the directory is not a root, then the directory is combined as previously with only the file name and no path information from the debug info. For cross compiling from Windows to Unix systems the debugger no longer strips the path information from the debug info. It now only strips the "drive" information and leaves the path. Drive information is c: and UNC server names \\MyServer. This change is designed to work in conjunction with the above directory path list change. Info can be found in the context help in the debugger options dialog.

*************** Build 29, April 29 2004 ***************
The compiler SYM file version number has been changed. All source code will need to be recompiled.

Added three new linker options for Win32 programs. Large address aware, swap run removable and swap run net.

The compiler no longer generates a case warning when a function procedure that returns a subrange type is used in the case expression and the case has selectors for the whole subrange type range. ISO M2 stipulates that a function call that returns a subrange has the type of the host of the subrange type. We changed the compiler to still obey those rules but avoid the warning generation in this circumstance.

Previously the compiler did not evaluate floating point to integer conversions of compile time expressions if assignment checking was enabled. For example CONST foo = INT(2.0 * 3.2). The compiler now always evaluates the above expression at compile time.

The ValidVersionTags feature is no longer optional. The compiler now enforces the fact that all version tags used in source code are either defined and/or listed as a valid version tag. If you add a version tag in the environment either via the version tags dialog, via the script procedures or via a compiler directive then those tags are automatically added to the valid version tags list. The ValidVersionTags feature was initially optional when we added the feature because no existing code used the feature at the time. That was over 5 years ago. You really want the ValidVersionTags feature since it forces a compilation error when you mistype a version tag identifier in your source code.

Fixed a compiler symbol scoping bug within CLASS types regarding a parameter in a class method and other class symbols. The compiler gave a compilation error on code it should not have.

We missed adjusting some example programs to the RTL changes in build 28. The FormEditUI.res was missing from the installation.

We eliminated the old unused debug formats, TurboDebug and Watcom.

Fixed a compiler code generation bug in 32-bit mode on the LEA instruction. If code had 16-bit operands with a sequence of add/mul/shift that the peephole optimizer could convert to a single LEA instruction and the result of the 16-bit computation was converted to a 32-bit value before being assigned. Then the assembler had a bug that it did not generate the proper prefix to make the LEA have a 16-bit result operand. Normal code could never have a 16-bit LEA result. Very obscure situation. A rare situation, an 8+ year old bug.

Fixed a compiler internal error when the peephole optimizer propagated a byte constant into a byte register push instruction. The assembler did not like a byte operand on a literal push. The code generator never output such a thing. Very obscure situation. A rare situation, a 12+ year old bug.

Fixed a compiler internal error when accessing CLASS structured constants via the SELF symbol.
 
The Win32 implementation of WinShell had a problem with accelerators if the main window was a splitter client.

The editor symbol matching feature did not work properly when positioned on a %ELSE token.

The enable/disable of menu items in environment editor window which were converted to top level windows did not work properly. This affected editor cut and paste operations.

Adjusted WinShell.PopupMenu(Handle) to be more convenient with MDI style window menu handling. Specifically enable/disable and check/uncheck of menu items.

The Win32 WinShell TreeClient window type did not display the + buttons at the root level if the lines option was FALSE.

Pressing the Backspace or Enter key while text is marked did not operate in the same way as most text editors. This assumes the replace selected text feature has not been disabled and the persistent blocks feature has not been enabled.

Fixed a problem with the Win32 implementation of the Terminal module Reset procedure.

Fixed a compiler code generation problem with the order of calling module initialization code. It would be too lengthy and/or difficult to describe here the exact situation(s) necessary for the problem to show itself.

Fixed bugs in the ComplexMath.arg and LongComplexMath.arg functions.

Fixed a problem in some Win32 Ole2.def procedure headers related to variant operators. The HRESULT was missing as the procedure result type.

Added new procedures to the BitVectors module. FindSetBitsEx, FindClearBitsEx.

When extended syntax is set to No, the compiler gave a compilation error on CAST(Type, ORD(NumericLiteral)). E.g. CAST(BYTE, ORD(55)). It should not have.

The WinShell.ImageListLoadImage procedure has been modified to allow adding non-resource bitmap images.

WinShell has been modified to support returning keyboard entry for the List and Tree client window types. This can allow you to perform text matching for selection of client item(s). This feature is enabled via the ClientCreateData record. Also see the comments on the WSM_KEY message.

The compiler did not correctly handle anonymous enumeration types in a VAR declaration inside a CLASS declaration.

When a script was running but the build status dialog was not yet displayed the environment would still respond to keystrokes. This is bad especially if you press enter and the selected module is an imported project.

The SMTP module has been updated.

Fixed a ResEdit problem with the status window display when editing a dialog and then using the Edit Symbols dialog. The status window field(s) were not properly updated after the edit Symbols dialog was closed.

ResEdit now has a delete confirmation dialog when using the Delete key to delete a resource from the resource picklist window.

*************** Build 28, January 16 2004 ***************
Fixed a problem in the Unix versions of the debugger. Clicking on a button in the breakpoints dialog would cause two actions to occur if the button did not have the keyboard focus. This happened in some other debugger dialogs as well.

The compiler recovers (resynchronizes) from syntax errors in a CASE selector expression better.

Fixed a compiler compilation error on CLASS types when an object dereferenced a field of a given name and another CLASS type, of the same name, was declared/imported in the same module. The compiler gave a compilation error thinking that you were trying to statically access a method of a class using the class override syntax. The compiler got confused because the method and another CLASS had the same name. An ambiguity can still exist due to the language definition, but the fix reduces this probability.

Fixed a bug in the resource editor importing of text string table. The escape characters were not properly handled.

BasicDialogs.PromptChooseFont now allows disabling of the font size selection control and allowing selection of only scalable fonts (TrueType, Type 1).

WinShell.FontInfo now supports specifying fonts in 1/10 point increments. You will have to update your code to recognize this change. BasicDialogs and TextWindows have been updated to this new standard.

Fixed a problem with the Italic and Bold font check boxes in the editor and project display options. Changing the option did not "stick".

Fixed a problem with the compiler where it could give a compilation error where it should not have. The problem could happen when a MACRO procedure from a def was imported into a module and a data type or variable had the same name as the macro inline procedure.

Added new APIs to WinShell. InitClientCreateData(important), SetDefaultDrawContext(useful), RunWindowModalLoop, QuitWindowModalLoop, ListClientGetItemText, ListClientFindItemByData, ListClientSetSortColumn, ListClientGetSortColumn, ListClientSetItemImage, CreateImageList, DestroyImageList, ImageListLoadImage, TreeClientRemoveAllNodes, TreeClientFreezeDisplay, ExecuteFormEditorMenuCommand, EnableFormEditorMenuItems, ControlSetEditable, ControlSetItemData, ControlGetItemData, ControlFindItemByData, ControlAppendText, ControlAppendLine, ControlGetLineCount, ControlGetLineLength, ControlGetLineText, ControlSetLineText, ControlRemoveLine, ControlMakeCaretVisible, ControlGetSelectedItemCount, ControlGetSelectedItems.

WinShell. Added a new message, WSM_TERMIANTEAPP. ControlTypes.TextEdit now has a password attribute. ControlTypes.ListBox now has a multiSelect attribute. The ClientCreateData record has new option fields for some client types.

Changed the WinShell.ControlProperties record with regards to GroupBox controls. They are now container controls with children. We made this change because it is nicer and it helps the Macintosh implementation of WinShell.

WinShell. Changed the menu id numbers for the FormClient form editor menu. We made this change to make the id numbers less likely to conflict with menu id number you might use in your own menus.

Changed the name of the DlgShell.IncDec control name to SpinButton so that it is consistent with the WinShell naming of the same control type.

WinShell. Changed the ColumnInfo record type into ControlColumnInfo and ClientColumnInfo. This allows the ListClient window type to have its own column description record so that it can add new features which a form or dialog column list box does not use.

WinShell. The ListClient window type now supports sorting, including user selectable, on a per column basis. It now supports three column content types. Text, Bitmap and Bitmap+Text. The procedures ListClientSetItemText and ListClientGetItemText have been changed to ListClientSetItem and ListClientGetItem to support the new modes.

Removed all the simple test/demo programs in the ShellDemo example directory in favor of a single program, TestWinShell, demonstrating most WinShell, DlgShell, BasicDialogs and TextWindows features.

Fixed a compiler bug with inlining a procedure which contains a GUARD statement. The compiler would crash with an access violation when compiling a procedure with a GUARD that will be inlined.

Added a new module, SMTP, to the RTL. This is a simple SMTP email interface module.

Improved the quality of WinShell.ScaleDrawableArea when using the InterpLowQualityMode on Windows.

We systematically converted ARRAY OF LOC/BYTE parameters to type ADDRESS on procedures in the RTL where the HIGH bound was ignored/unused and a size parameter associated with the array was used instead. This is a stupid use of open arrays and may cause problems porting an RTL source module to some other compiler. The code can compile but might not execute properly. We discovered this the hard way. This change affects many modules in the RTL. MemUtils, GenCRC, all the encryption related modules, NamedPipes(read/write), Pipes(read/write) and FileFunc(ReadBlock/WriteBlock). Yes, it is a pain adjusting to this. We know. We had to change all of the RTL and our own programs.
 
Fixed a compiler code generation bug with parameter copies of multi-dimensional open array parameters.

Fixed a compiler compilation problem with some instances of CLASS types with debug information enabled. The compiler crashed with a stack overflow error.

Fixed a bug in the debugger where a string search of the source code would not match multiple occurrences of the search string on a single line of code.

Fixed a compiler peephole lifetime optimization bug using the cmp instruction. Multiple register propagation optimizations had to occur on the cmp instruction resulting in a specific register usage for the bug to expose itself.

The compiler now supports RECORD constant constructors containing bitfields.

Fixed an access violation that can occur in the environment in situations where the environment scans a source file for the module header to determine if a source file is an implementation or program module type. The access violation could occur when a libneeded or resource directive was in the source before the module header. This problem is fixed but you should never use directive outside of the module definition since those directives are outside of the module and are therefore not a part of the module. For the foreseeable future our compiler associates those directives with the module the follows. However, being strict with directive usage is always best.

The editor block matching feature will now stop on the EXCEPT part of a procedure.

The compiler did not accept forward referenced types in procedure type declarations. For example
TYPE
    Reader = PROCEDURE (Data);
    Data = RECORD END;


*************** Build 27, August 8 2003 ***************
Updated the OpenGL defs files. Various "vector" APIs did not have one of their parameters defined as Modula-2 ARRAY OF parameter types.

Added LONGCARD support to FormatString.FormatString. Both decimal and hexadecimal output is supported.

The linker now reads an additional Gnu linker script keyword. This was necessary because Linux RedHat 9 added an additional keyword to the pseudo script files.

WinShell now correctly uses the default language character set. Previously it was fixed on the ANSI character set. This affects the status line, tab control tabs and FormClient controls.

Fixed a debugger bug with the display of expanded string types after you modified the string using the whole string display node. The expanded array character elements were not updated after the whole array modification.

Fixed a debugger problem with modifying single character string variables.

Changed the method of reset for the DlgShell.SetupStringMatch API.

The compiler could give an incorrect error location when an error existed in a conditional compilation directive.

Fixed a compiler bug with CLASS types and data defined in the implementation portion of the class type. You needed two class types in the same module, the second inherits from the first. Both have hidden data in the implementation declaration of the CLASS type. Data accesses to the first CLASS type hidden data from the second CLASS type would be invalid.

The WinShell module has some changes in some APIs. This is because of the port to the Macintosh interface using the p1 compiler. Changes have been made to.
1. Some clipboard APIs.
2. GetSubMenu.
3. IsUserMessageWaiting.
4. ChangeDrawableBitmap has been eliminated.
5. Added DrawBitmap and DrawBitmapRect APIs.
6. Palette support has been removed. The Unix implementation did not implement this, so we dropped this feature since it was not fully portable. Also <= 256 color displays are a thing of the past.
7. We extended the resource name identifier definition. This does not change existing code, but the change adds portability and some capability.
8. GetCursorPos has been eliminated. Just remember the last mouse position from a mouse move message.

The DlgShell MlePositionToBottom API has been changed to MlePositionCaret. The API is now generalized.

*************** Build 26, April 30 2003 ***************

!!! You will have to recreate your projects with this version.

Fixed an environment file date/time problem, related to daylight savings time, where Win9x was behaving badly in accessing a networked NTFS volume. The problem occurred if you worked on a project stored on the NTFS volume and switched between a Win9x machine and an NT machine. The Win9x machine thought the date/time had changed when the NT machine had modified something in the project. We worked around this problem, but this required storing additional date/time information on a per module basis. We now track both the local and UTC file date/times. Previously we only tracked the UTC date/time, plus a FAT/FAT32 file system fudge factor. We had to change the project file version number due to the data structure change.

The profile analyzer was trying to load DLLs, which had no debug information. This would include systems DLLs. This was annoying. Since the debugger does the profile sampling an internal change in the debugger affected the analyzer and it was not updated to handle this change.

Fixed a bug in RealStr.RealToFixed and LongStr.RealToFixed that was introduced by changes made in Build 25.

The linker now understands the special Win32 DLL import resolution format used by VC++. It is still more efficient to convert the file. The linker basically does this on each use.

Added new Win32 interface files. SimpleMAPI.def and LDAP.def.

The resource editor now provides easier control for enabling or disabling the specification of a font used in a dialog. In addition, the pseudo font "MS Shell Dlg" is now supported by the resource editor. "MS Shell Dlg" is now the default font specified for new dialogs.

Fixed an access violation in the resource editor in the Order/Group dialog when you had a control with a large amount of text, greater than ~220 characters.

Fixed a bug in the DlgShell NumberEdit control validation.

Added support to the WinShell TreeClient window to allow having no folder bitmaps displayed in the window.

New WinShell module API. SetMenuItemStr, TreeClientGetParentNode, TreeClientGetNodeText, TreeClientSortNode. Support for dashed lines in draw contexts.

New DlgShell module API. GetControlHandle, SetItemText, GetItemText. SetColumnText and ChangeItem have been removed in favor of the more generalized SetItemText API.

Improved support in the DlgShell SetSelected and SetText APIs for multiple selection list boxes.

New Socket module API. GetHostName, SetSocketOptions.

An editor quirk has been fixed. When selecting a single line by clicking in the gutter, a subsequent copy/cut operation did not have a line break included with the line text.

We changed the implementation of the synchronization in the Unix Threads module implementation. The new Linux NPTL threads library broke our old implementation. Redhat 9 is the first known distribution with the NPTL threads library.

*************** Build 25, March 19 2003 ***************
Fixed a problem with the 16-bit DOS "overlay" compiler option code generation. An internal error occurred when compiling code. This portion of the compiler was not updated when we changed the structured return code generation in Build 7.

Fixed a problem with 16-bit structured function return code generation.

The 16-bit runtime library now defaults to using the Medium constant memory model. This should help alleviate DGROUP overflow issues.

We eliminated the small modeless "find" dialog, which was used after find operations in both the environment editor and debugger. This was done because the dialog did not add much useful functionality and its existence is different from other editors out there. The replace operation in the editor still uses a small modeless dialog to allow selective replace operation. This change may cause some initial "pain" until you get used to the change.

The compiler generated incorrect code when using BITFIELDs in a WITH statement and the bitfield had a record field allocation offset of zero.

The environment was not passing the LIB/SO directory list to external linkers in the %S replacement parameter.

The resource editor ICON control type now supports ICONs and BITMAPs.

Additions to WinShell.
1)Loading of JPEG, PNG, TIFF and GIF resources and files is now implemented on Win32. This feature uses GDI+. Unix systems already had this ability.
2) A new RGB drawable type.
3) ScaleDrawableArea API.
4) On Unix systems the mouse wheel is now supported.

DlgShell now supports a bitmap display control.

The Unix versions of the environment and debugger now support the mouse wheel for scrolling if the X server supports the defacto standard for the use of the mouse wheel.

We have converted the flat (non C++) Win32 GDI+ API header files to DEF files. See Gdiplus.def and GdiplusFlat.def.

The linker was generating a DLL resolution LIB file on Unix systems when it should not have.

On Unix systems, the linker no longer places references to unused shared object files passed to the linker in the linked executable file. A shared object is considered unused unless one or more symbols from the file are used in the linked code.

We improved the function of LIB/SO paths and imported projects. Imported projects that have a LIB/SO path value now have their paths passed to the linker. Previously only the current project LIB/SO path was sent to the linker, therefore possibly forcing you to copy paths from imported projects into the project that imported them.

Fixed a peephole lifetime optimization bug with converting a non byte type to a byte type and then immediately converting the byte value to a non byte type in the same expression. The original non byte type must have been allocated to the SI or DI register and the register used during the conversions must have been dead after its use in the conversion expressions. A rather particular scenario.

Two new environment script functions have been added. FileExists and DirExists.

*************** Build 24, Feb 4 2003 ***************
The compiler now gives a compilation error when the UNREFERENCED_PARAMETER feature is used on a parameter that is referenced in the code.

The compiler has a couple of code generation improvements. One related to floating point array subscript code and the other related to the WITH statement.

Changed the compiler syntax for assembly procedure register variable assignment.

Fixed an environment display problem when opening a project that had modules that were imported from a project and then had source code, or the reverse. The display was not updated to display the greater or lesser number of modules.

Adjusted the environment dependency scanner algorithm to properly follow the documentation with regards to search order. This problem would only show itself when you had the same module exported from more than one project, and those projects are all imported into the same project. The module may not have been imported from the last imported project containing the module.

Fixed an uninitialized variable bug in VLI.FromHexString.

The group box control in the resource editor now has the WS_GROUP style set by default.

On the compiler SrcOpt and AsmOpen options panels the controls were not all enabled/disabled when the "yes" and "No" options were checked.

We increased the size of the stack reserve in the environment and debugger executables. Apparently, HTML Help is a real stack pig when performing some tasks.

Fixed a couple of problems with the install program. The environment help reference in the system "programs" menu still referenced the old Winhelp file env.hlp. The ".sbs" script file association did not function properly.

The resource editor did not disable the resource pick list window, when some modal dialogs were shown. This would allow you to mistakenly open a new resource while the modal dialog was still running. Internal data structures would become trashed in this situation.

Fixed a bug in the "FROM Module IMPORT * EXCEPT ..." extended syntax feature. If a symbol in the EXCEPT list was an enumeration type name then the symbol in the DEF file just after the enumeration was not imported.

Fixed a bug in the resource editor horizontal tab control styles dialog. The "styles" radio button group was not properly grouped, therefore the buttons were not mutually exclusive if you clicked the "buttons" style.

The resource editor now supports the BS_PUSHLIKE style for RadioButton controls.

Fixed a problem with the environment script creation feature. The text for the PrjStopBuildOnError option was not output.

The compiler typed pointers option was broken at some point. It is now working again.

We changed the debugger algorithm for forcibly stopping a program on NT/2k/XP systems. The algorithm now works in all circumstances, even deadlocked threads.

The WinShell GUI encapsulation module has been significantly enhanced. The Splitter, Tree and List clients are now implemented for all supported targets. The Win32 implementation has the Form client implemented, including the form editor.

Fixed a Win32 WinShell bug when TabClient windows were children of SplitterClient windows. The tab client children were not resized properly at times and the active child window of the tab client became hidden when the splitter window split percentage changed.

*************** Build 23, October 11, 2002 ***************
The compiler now ignores the "Check modules" option for modules, which are imported from a DLL project.

The debugger no longer creates its various windows as "owned" windows of the main window. A new menu, Window, has been added to control bringing specific windows to the foreground.

Fixed a peephole optimizer bug related to conversions to byte types from larger whole number types.

Fixed a compiler parser bug where it did not allow assigning a procedure variable to another procedure variable in a valid situation.

*************** Build 22, September 20, 2002 ***************
A new option has been added to the environment. You can select whether a build stops on the first compilation error, or continues compiling available files. The feature for continuing compilation after errors was added in build 7.

We have switched our help files for the environment, debugger, profiler and language reference from the WinHelp format to the HtmlHelp format.

Made some changes in the parameter structure of some procedures in the VLI, RSA and CryptEncode modules. These changes were made in increase flexibility and safety.

Added a couple of new modules to the crypto library modules. HMAC and CryptKey (key derivation functions).

Fixed a compiler bug with local modules defined inside a procedure. A compilation error occurred when importing a symbol that was imported at the outermost level.

Fix. RichEdit controls were listed in resource editor HTML reports as "status bar".

The resource editor now supports the BS_PUSHLIKE style for CheckBox controls.

The resource editor now allows setting the text alignment options (left,center,right) on single line edit controls.
 
Fixed a compiler code generation bug related to procedure inlining. You needed two consecutive loops in a procedure that was inlined and the first loop test code needed to be jump chained to the loop test code of the second loop and the second loop needed to have loop invariants.

Fixed a resource editor bug that occurred while scanning a DEF file and the file contained two consecutive comments with no characters in-between the two comments.

Added a new module to the Win32 RTL, SplitterControl. With this control, you can split a window into multiple views.

The WinShell GUI encapsulation module has been mildly restructured to be able to add additional capability. Additional capability has been added.

You can now split an environment editor window into two views and display/edit two separate locations of the same file. The Edit menu, Split Window command controls splitting and unsplitting.

Fixed a linker bug with linker user defined resource types when two consecutive unique user resource types had the same resource identifiers.

*************** Build 21, June 12 2002 ***************
The performance of the new VLI module has been dramatically improved. Other changes to VLI include additional and modified APIs.

Added a new module to the RTL, RSA. This module implements the RSA public/private key encryption algorithm.

Added a new module to the RTL, CryptEncode. This module implements various data block encoding/decoding algorithms used with various encryption algorithms.

We fixed a compiler bug with parsing enumeration types with exactly 256 elements. Build 4, with it's extensions for easy porting of C code, broke the behavior that such a type should be stored in 1 byte rather than the size of a CARDINAL.

New module added to the RTL, Money. This module implements an integer fixed point type suitable for monetary computations. It has greater precision and accuracy than a double precision floating point type (LONGREAL).

Fixed a compiler bug with the SYSTEM.CAST function, to a byte type, when passed an expression of a larger type with an operator (+, -). The code generation would fail *if* the operator resulted in the SI or DI registers.

The resource editor now allows you to set the Center and Right alignment options for a single line edit control.

Fixed a compiler bug where an ASCII string constant was not converted to a Unicode string when passed to a Unicode array parameter when the default CHAR implementation option was set to ASCII. Literal constants did not have the problem.

Fixed a GDI resource leak in the GUI programs. A 16x16 icon was leaked for each window closed.

*************** Build 20, May 9 2002 ***************

Fixed a bug in the compiler that could cause an internal error while compiling a file. The bug was introduced with common subexpression optimization "adjustments" added in build 16/17.

Fixed a bug in the compiler that could cause an access violation while compiling a file. The bug was introduced with the compiler inlining improvement added in build 18.

An updated version of the IDL compiler is included.

New module has been added to the RTL, VLI. This module implements arbitrary sized integers. VLI = Very large integer.

*************** Build 19, April 26 2002 ***************
The resource editor edit symbols dialog limited the ID value field to 7 characters, which was limiting when you were defining a symbol as an offset from another symbol. The other symbol name needed to be 7 characters or less.

Bug fix in the environment file change checking on the FAT file system, with regards to daylight savings time. The problem only occurred on obj/lib/sym files. The bug was that the environment thought a file had changed when this was not the case.

We have converted the Win32 shlobj.h and shobjidl.h files to Modula-2. The definitions are in the SHLOBJ.DEF module.

The performance of various procedures in the Strings module has been improved.

Fixed a bug with record constants in a DEF module and imported into another module. The record type must have had at least one compiler padded field, in the middle of the record. Also, the record must have been smaller than 16 bytes, or contained fixups (the use of ADR as the value for a field).

*************** Build 18, March 14 2002 ***************
Added a new extension to the SYSTEM module, OutputDebugMessage. See the online help for more information.

Fixed a bug in the Win32/Unix debugger when you stepped into a DLL that did not have debug info. The problem caused the debugger to generate an unknown breakpoint message. A problem also existed if you set breakpoints in DLLs that had no debug info.

Fixed a couple of bugs with the environment script creation feature. A problem existed with certain linker options that took a string parameter, where the string was not surrounded with the quote character. Also, the new linker options for symbol table generation (Unix) and Virtual memory for X32 programs were not correctly output.

The compiler will now generate subscript checking for the upper index of array slice expressions.

You can now use environment variables in the directory option search paths. This can be useful on Unix systems where you might want to use the HOME environment variable. Generally, it is best to use relative paths, but this adds additional flexibility.

Various large set operators are more efficient.

Added a feature to the DlgShell example code that helps in porting existing Windows code to the portable DlgShell interface. See SetControlIdMode in the definition module.

The GUI encapsulation modules (WinShell, DlgShell, BasicDialogs, TextWindows) are now in the RTL project and no longer in the examples directory. They are only available for Win32 and Unix targets. As such, the Examples\Winshell directory no longer exists and examples\shelldemo has been created instead. This directory contains example programs using the encapsulation modules. The largest demo being the text editor used by the environment.

Fixed a bug in the FormatDT.StringToDateTime when four digit years were entered. This bug also affected the environment "List Altered" feature.

New modules added to the RTL. MD5, SHA1, SHA256, SHA384, SHA512, AreSee4, DES, Blowfish, AES. These are used in cryptography and the names of the modules should tell you what they do. Thanks to Egbert van der Haring for providing the MD5 and SHA1 code.
 
Changed how open array constants are parsed in the compiler and the restrictions that previously applied to open array constants have been lifted.

Improved how procedures with VAR parameters are inlined. Potentially better code generation is a result.

More structured constants should now be viewable as variables in the debugger.

Fixed a bug in debug information generation when you were using the compiler extension for supporting translation of C language enumeration types and one of the enumeration ids had a value greater than 65535. The result of this bug was a debug packing error.

Fixed a bug in the environment script parser. If you entered a string literal longer than 255 characters the string was truncated without an error message from the parser.

The compiler optimizer now treats variables in which you explicitly have taken the address of (ADR()) as volatile "hands off" variables. Generally, when people code this way they are aliasing the variable and the global/local aliasing option had severe effects on code generation. You now only need to use the Alias Global option when you pass a global to a procedure and access the same global directly within the same procedure. The context help for the Alias global option has been updated to reflect this.

The debugger module list in the View module dialog is no longer sorted case sensitively.

The SYSTEM.OFFS compiler extension now accepts variables in addition to types.

The SIZE function (MIN and MAX also) have been extended to allow you to reference fields in a record type. SIZE(RecordVariable.field) has always been allowed and now SIZE(RecordType.field) is allowed.

Fixed a bug with the 64-bit SWAPENDIAN operator when used with indirect memory references. In some circumstances, incorrect code could be generated.

*************** Build 17, December 18 2001 ***************
Fixed a bug with LIB/SO imports across project boundaries where the file extension would get "lost". This would cause a linker error.

The ExStorage module has a new feature regarding how memory allocations larger than the chunk size are allocated. See the help file How To->Memory Management topic or ExStorage.SetChunkSize for additional info.

The SetUnhandledExceptionProc feature added in build 16 now allows you to pass it a procedure that has the EXPORT attribute.

Virtual memory support is now available for 32-bit DOS extended programs.

Fixed a bug in the 16-bit startup code. SYSTEM.IsInit and SYSTEM.IsThread were not initialized, which might cause portions of the RTL to not initialize themselves and cause the program to crash. The variables exist in 16-bit to simplify writing portable code across all targets our development system supports.

Fixed a compiler access violation that occurred when you used the new compiler enumeration extension for porting C header files, and the enumeration had "holes" of a certain size and you turned on symbolic debug information.

The resource editor now supports importing string resources. The format for import is the same format used for export.

The resource editor now allows you to apply the BS_NOTIFY style to Push, Radio and Check button controls.

The maximum size of individual strings in string resources has been increased in the resource editor.

Fixed an access violation in the resource editor that can happen when deleting an existing string resource and then creating a new string resource without having selected any other resource in the resource picklist window in between the delete and create.

The example GUI encapsulation modules (WinShell, DlgShell) now encapsulate the concept of "help" in a portable manner.

We have restructured how we do help and context help to support having help on Unix systems. If you notice any anomalies please let us know.

Fixed an environment problem when you placed the Libneeded directive before the module header in a source file. The environment had a problem with the directive since it did not yet know the type of file it was parsing because the directive was before the module header.

Fixed a 16-bit compiler bug with generic instances that have procedure parameters. The compiler did not force the generic procedure parameter to be a far procedure and thus you could get a compilation error for valid code. If the procedure were already far then no error would result.

The popup menu in the editor was not positioned correctly when you clicked beyond the end (to the right) of the text on a line.

Fixed a compiler bug with procedure inlining and VAR parameters. You have two procedures A and B. A has a VAR parameter and is inlined into B. B has a VAR parameter and is passing this same VAR parameter to the A VAR parameter. B is also inlined into another procedure. At this point the compiler did not correctly "unVAR" the parameter sequence during the inlining. This created an alias situation with the original variable passed to the B VAR parameter. This alias situation might end up resulting in incorrect code generation if the alias causes problems in code optimization. The code must be such that a problem will manifest itself in an alias situation. Most of the time this is not the case. If optimization was off, or the Alias global/local compiler option was set, the alias situation cannot manifest itself. This sequence of events is obviously quite rare since this bug has existed since V2 (~12 years).

Fixed a problem in the debugger where the array subscript values were off by one when displaying an array slice. The data and addresses displayed were correct, only the subscript values (inside the []) were off.

*************** Build 16, October 30 2001 ***************

Added a new feature, available on the Tools menu, that lists project statistics such as number of modules, line of code, build time, compilation performance.

Added a new module to the RTL, Timers. It is available on Win32 and Unix systems. It provides a portable encapsulation for creating timers not associated with GUI windows.

Added a new compiler extension to the SYSTEM module, SetUnhandledExceptionProc. With this procedure you can set a procedure that is called when an exception goers unhandled. See the SYSTEM module documentation in the help file for more information.

A "Close window" command has been added to the editor context popup menu.

The script system has a new procedure, RemoveModuleEx.

Ctrl+Tab is now an accelerator to cycle through activating each environment child window from left to right.

The procedure view mode in the editor no longer includes procedures that are commented out. The scroll bar in the procedure view was incorrect when initially displayed.

Improved the support for object library files (LIB) that are imported across project boundaries. You no longer have to duplicate the LIB/SO directory path of the imported project into the projects that import it. Additionally, the environment always searched the current directory for these files. This is no longer the case. The only assumed directories are the "system" library search directories.
 
Added Ctrl+F4 as an accelerator for closing an editor window. This is for those who were used to the Windows accelerator (not Stony Brook) for closing MDI windows when our environment used the Microsoft MDI model. This is considered an undocumented accelerator and it might disappear IF we find some other use for the accelerator key. Along with this, we added a documented accelerator for closing an edit window. We changed the accelerator for the global and local options dialogs. Now the same accelerator works for both dialogs in the project and editor windows. Previously only the local option accelerator was available in the editor.

The version tags dialog/system now lets you have the version tags string broken into multiple lines. You can force a line break via Ctrl+Enter.

Fixed a problem in the environment where a toolbar button bitmap might have become "trashed" on Win9x or disappear on NT/2k. This fix corrected the one situation we could duplicate. The environment was sharing bitmap handles with more than one toolbar, and it no longer does this.

In build 12 we increased the size of various string options. For version tags some parts of the environment had local variables that were still of the old size. This has been corrected.

The editor toplevel window feature did not function properly when the window converted to toplevel was not the last tab.

Fixed an environment problem where if you had a foreign module source (ASM or C) and you had a normal implementation module it was possible for both the IMP and ASM/C modules to be added to the project. When you tried to remove the excess module, presumably the foreign module, the environment crashed during the remove. This was because the "module" had two implementations, one in M2 and the other a foreign implementation.

Fixed some problems in the environment project window right click popup menu.

Fixed a compiler debug info generation problem with local modules that have initialization and termination (FINALLY) code. The problem caused a linker error.

Fixed a compiler error where valid M2 code got a compilation error. The code was when two local modules at the same scope level referred to each others exported symbols. Version 3 accepted this code and it became broken at some point in V4.

*************** Build 15, October 5 2001 ***************

The ExStorage module MemoryInUseEx, MaxMemoryUsedEx and HeapMemoryEx calls are now able to return a value which is the sum of all active heaps being managed by ExStorage.

In the project options dialog you can now select the position of the window "tabs" of the project and editor windows. This option is a global per user option, meaning all projects will use the selected format.

You can now use the profile analyzer on programs that do not have debug information. With this change, you can profile a DLL with debug information attached to some program.

You can now debug GENERIC module instances symbolically at the source level.

Fixed a compiler bug that generated a compilation error for valid code. If you had a local module nested within a program or implementation module and you had an otherwise legal FOR loop control variable for the module initialization code you got a compilation error stating the control variable was invalid. Local modules within procedures did not have this problem.

The environment script generator did not output a leading '0' character for those entities (2) which it writes in hexadecimal format. The '0' character is needed when the first digit is 'a'..'f'.

Fixed a bug in the Threads.CreateThread procedure. The bug could only manifest itself if the created thread fully executed and terminated before the call to CreateThread returned.

A linker bug with exporting symbols from a Win32 DLL via a linker definition file (EDF) has been fixed. The bug occurred when the name listed in the EDF file did not include the typical Win32 public symbol decorations and the public symbol did have these decorations. Multiple symbols must have existed where the name was mostly the same. For example, the number "2" at the end of one of the names where otherwise the names were identical. No problem existed when using the compiler EXPORTS declaration to export symbols.

When linking Win32 or Win16 DLLs the environment now verifies that the linker generated a resolution import library. The environment also checks the existence of the import library when making a determination if the DLL needs to be linked. (If you delete the .lib and not the .dll the environment will now link the DLL since no .lib also exists for the DLL)

*************** Build 14, August 29 2001 ***************
Added a "Close all" menu item to the editor window file menu.

We updated the environment's module dependency sort to support object library and shared object file linking order. These modules are specified using the Libneeded compiler directive. The order the Libneeded directives appear in a source file determines the link order of the imported libraries. Link order is normally not important, but some libraries might have linking dependencies. If you have a pre-existing project, you should mark the module(s) that import multiple libraries as altered so that the environment will rescan the module dependencies.

Fixed a problem in the Win32 debugger for code that had enabled compiler runtime checking code generation and a runtime error occurred. The debugger would stop at the checking error, but if you tried to release the exception because you had exception handler(s) you wanted to execute, the operating system did not raise the exception.

Fixed a bug with linker debug information generation. The bug did not seem to cause any problems in the debugger. At least none were observed or reported.

Fixed a compiler register allocation bug with regards to function procedure variables. Non function procedure variables were fine. If a procedure variable expression became enregistered then it was possible given the appropriate circumstances for incorrect code to be generated. Object oriented code had a greater opportunity to stumble across this bug because all procedures in CLASS types are procedure variables. An interesting note, this bug has been in the compiler since inception (> 10 years).

*************** Build 13, Friday 13 2001 ***************
Due to bad luck, this release was lost.

*************** Build 12, August 17 2001 ***************

The API of the Threads.Barrier synchronization object has been changed. It no longer requires that threads be associated with a barrier before they can use the barrier. Additional APIs have been added to the Barrier object.

Fixed a compiler bug that did not allow CLASS types to be declared in a generic module. A compilation error occurred when you refined the generic module.

The usage of the leak detection feature of the ExStorage module has been enhanced.

We increased the size of various string options stored in the project file. This caused a project version number change. Your projects will need to be recreated.

Updated the version tags user interface to make it easier to edit a larger number of version tags.

Fixed a bug in the Peephole optimizer when using multiple small record types in specific ways. Two local record variables needed to be assigned to each other in a specific way without intervening code. Then in later code, with no intervening code, one of the records had to be used in certain ways to expose the bug.

Fixed an editor bug where you could not clear/remove the fourth position marker.

*************** Build 11, July 12 2001 ***************

Fixed an environment bug where you had a project with one or more modules open, and you closed the project with the source files open. The source module must have no dependents in the project. You then use a script file that does a "make" on the project, the previously open modules are automatically opened, however you have deleted the source files before running the "make" thus making the modules not found. The background source file scanners in the environment notice a completely unfound module with no dependents and automatically deletes these module(s). Meanwhile in parallel, the editor was opening the module, which results in a blank edit window since no source was found. The editor thinks this is a "module" and not just some "file". This is the source of the problem. I will stop here with the details. The auto open feature now requires the module be "found" to open a module. The auto open feature now waits for background operations to complete. The auto open feature will not open modules if a script is or is about to be executed.

Fixed a problem with the "Find" dialogs in editor windows that were converted to top level windows. The dialog may have been centered over the wrong window (top level vs. main)

A new option has been added to the project options. It allows you to change the value used for the compiler initialized data option.

The editor "top level window" feature now creates the top level window as an independent window (previously it was an "owned" window).

The 16-bit DOS and 32-bit DOS extended debuggers needed to be updated to accept the '-' character as command switch indicators. The environment no longer uses '/' with switches for portability reasons.

Fixed a lockup bug in the BitVectors.Duplicate procedure.

Fixed a lockup bug with the History Archive feature of the environment. Yes, this bug was caused by the BitVectors bug.

Fixed a bug in the built-in assembler syntax where it did not give a compilation error for the following syntax [4 * ecx]. When using Intel scaled index byte addressing you must use the * after the register to be scaled, [ecx * 4].

*************** Build 10, June 20 2001 ***************

Fixed a compiler bug where it did not report a syntax error in certain conditions with procedure attributes if certain reserved words were used as procedure attributes. For example
PROCEDURE it(a : CARDINAL) [Win32System, EXPORTS];
Exports is invalid but no compilation error was reported. If this was in a DEF file then an error probably occurred while importing the DEF into some other module including the implementation.

Fixed a compiler internal error when you passed and array slice expression to a NOHIGH array parameter. Normal array parameters always have high bounds, except those specially defined in operating system API definition files. Example
WINUSER.MessageBox(NIL, str[1..5], "", MB_OK);
In this case the slice expression is meaningless, except for the lower bound, since the high bound is unused on NOHIGH parameters. Also operating system strings parameters, the kind that use NOHIGH, expect to be null terminated and an array slice expression is not likely to be null terminated.

Fixed a problem with the linker when using Win32 identically named resources differing only in the language of the resource. The linker was not properly sorting the resources within the executable resource table.

When using the duplicate resource menu command the resource editor now keeps the language of the new resource the same as the duplicated resource. Previously the new resource language was always the current system default.

*************** Build 9, June 4 2001 ***************

Fixed a problem in the Win32 debugger while single stepping in source display mode where the debugger might "lose control" of the program. The debugger might not step properly and therefore you did not stop where you wanted to, or at all. If was most likely to happen at the bottom of a loop that was branching to the top. The bug was "added" in build 7.

Fixed a problem with the editor options dialog when using the Disable overwrite mode and Disable replace selected text options.

A file necessary for the 16 and 32-bit DOS floating point emulators was missing from the installation program.

The Win32 debugger can display a much larger number of modules in the "View module" dialog.

Fixed a bug with the history archive feature of the environment. It was broken. At some point the RTL PipedExec module became strict about its input handles and the environment source was not updated.

*************** Build 8, May 9 2001 ***************

The resource editor now uses the resource name as the default name when exporting bitmaps, icons and cursors.

A new feature added to the editor to display a marker at a specific column of text. You can use this to help format your text within a certain number of columns.

Toolbar bitmaps are now larger, 24x24, and some of the pictures have changed. If you have customized the project or editor window toolbars you may have to reformat them, or you can delete the registry keys,
EnvToolbar and EditorToolbar in
HKEY_CURRENT_USER\Software\Stony Brook\m2e
 
Fixed a windowing bug where partially displayed lines were left at the bottom of a window, such as in the editor.

The editor allowed an incorrect edit position when paging down to the end of file. The edit position may not have been at position 0 when the caret was beyond the last line of the file. Entering a character at this point caused an access violation.

Fixed a problem in the new editor Toplevel window feature that could cause an access violation.

*************** Build 7, April 27 2001 ***************

Due to some underlying structural changes it is suggested that this build be installed in a fresh directory or that previous installations be uninstalled first. Because of these changes the compiler SYM file and project file version numbers have changed.

The environment has a new look. You may want to check the help file Basics topic(s) for additional information.

The RichEdit.def has been updated to the 3.0 specification.

The resource editor can now create version 2.0-3.0 compatible RichEdit controls.

The compiler now performs internal floating point arithmetic in 64-bit LONGREAL format. Previously it used the Intel 80-bit format. This is not portable to other processors, therefore we made the change.

Fixed a resource editor bug when duplicating an Icon or Cursor resource and the duplicate language is different than the language of the source resource. The bug resulted in an access violation.

Two new functions have been added, to complement the SWAPENDIAN function (BIGENDIAN and LITTLEENDIAN). See the SYSTEM module help topic for more information.

The RTL source directory structure has been modified slightly to support processors other than the IA-32 compatible processors.

Fixed a bug with the compiler constant combining. This can only occur if you declare constants with fixups as elements of the constant. This means that one of the elements of the constant was the ADR(...) of some other constant or a variable. Another true constant with a value of zero, may combine with the fixup value, which at compile time is zero. The value of the constant at the fixup changes at link time due to the computation of the fixup.

We added a new compiler option to support using other tools on Unix platforms. The new option is Extended archive. Unfortunately, this required a project version number change. All projects will need to be rebuilt.

We added a new librarian option to support using other tools on Unix platforms. The new option is Extended archive.

Fixed a bug in the Threads module with regards to WaitForThreadTermination. If the wait was called immediately after thread creation there was a possibility that WaitForThreadTermination might think the thread was terminated, when in fact it had actually not executed a single instruction.

We added an additional parameter to CreateThread to signify if a thread is to be considered "waitable". If true then this means you intend to call WaitForThreadTermination on the thread.

We changed how LIB files are found. Each project now has an option in the directories option for a search path of directories to use to find object library and shared object files. If this option is blank or the file is not found, then the "system standard" directories are searched. This algorithm makes cross development, on Win32, for multiple Unix platforms easy. See the directories dialog help for more information.

The compiler bit-field packing algorithm has been changed. It now correctly duplicates the algorithm used by most C compilers, on both little and big endian processors.

The Win16 debugger has been separated from the Win32 debugger. It is now named sbdw16.exe.

FYI. The compiler executable file names have changed. They now identify the processor for which they generate code. m232.exe -> m2ia32.exe. m216.exe -> m2ia16.exe.

The compiler code generation for structured (record, array) function return types has changed slightly. It is now compatible with other processor architectures.

Environment bugfix. The accelerators on the project window file menu activated the incorrect functions.

The compiler "build all" commands no longer stop on the first compilation error. They now compile all files it is possible to compile before the build closes. All files with compilation errors will be opened in the editor to correct the error(s).

A new command has been added to the environment. This command allows you to remove all unused modules in a project. Unused modules are modules, which no other module in the project references. This command is on the module menu.

A new synchronization object, Barrier, has been added to the Threads module.

*************** Build 6, November 9 2000 ***************

Fixed a compiler bug that generated bad code. You needed an arithmetic expression of type CARDINAL or INTEGER. The expression needed to consist of multiple operators. The expression must have been assigned to either the SI or DI registers by the register allocator. This expression must have been converted to a single byte type in an assignment statement. You must have had assignment checking turned on (set to Yes). The assignment range checking operation got in the way of the register allocator protection from putting expressions resulting in bytes types into the SI or DI registers which cannot be accessed as bytes.

Fixed a compiler bug when the source code had a syntax error in the array subscript. The array element type needed a structured type of a specific size. If the array type was then subsequently used with a constant constructor, the compiler clobbered itself in the constant constructor due to the previous syntax error in the array type. We improved the compiler error recovery for array type declarations.

Build 5 introduced a bug with selecting fonts in the environment or debugger. This bug was introduced during restructuring as a part of our Unix portability project.

*************** Build 5, November 1 2000 ***************

Added a new compiler extension to allow procedure bodies inside DEFINITION modules. This amounts to something of a macro since the procedure code is inlined on each occurrence. Consult the language reference help file for more information.

A new compiler extension has been added to allow the declaration of bitfields inside record types. This allows better and easier conversion of C header files to Modula-2. Consult the language reference help file for more information.

The above changes required a compiler token change and therefore a SYM file version number change. This means your code will need to be recompiled.

A new synchronization object has been added to the Threads module. This object is similar to POSIX condition variables.

Moved the RwLock object from example code into the Threads module.

Enhanced the WaitForThreadTermination implementation with a RequireThreadWait API procedure.

Added a Thread termination notification API.

Two new modules have been added to the runtime library. They are only available for Win32 and Unix systems. ConfigSettings and Socket. ConfigSettings provides a portable interface for reading and writing program configuration settings. The ability to use global system wide and per user settings is supported. Socket is a portable implementation of the "sockets" API.

A linking bug with the V4 system and the 16-bit DOS virtual overlay manager has been fixed.

Object library (LIB) modules added to projects are now linked differently. They are linked much as resource files are linked. Consult the help file topics; Basics, linking your programs, and the compilers directives help topic, for further information.

A new option for creation of object library modules has been added (Include object libraries). The default behavior is the same as before. Consult the librarian options online help for further information.

Fixed a compiler bug that may occur when compiling and/or using generic modules. The compiler had an uninitialized variable and an internal error could occur.

Fixed a problem with class types and the ReadOnly reveal attribute. The compiler was too restrictive on subclass types when applying the read only attribute.

Fixed a bug in RealStr.RealToFixed when writing a value of zero with "place < 0". RealToFixed(0.0, -1, string). LongStr.RealToFixed did not have this problem.

We fixed an environment bug with program files using short file names and executing the debugger from the environment. The debugger would report it could not find the program. This could only happen in Win32 projects.

Fixed a compiler bug with CLASS types and non overridden procedures declared in the implementation module. The CLASS must have been defined in the definition module and the procedure was not defined in the CLASS definition and was defined elsewhere in the class hierarchy. The compiler did not give a compilation error, when it should have.

The debugger no longer uses MDI for its windows. Each window in the debugger is an independent window.

The WinShell/DlgShell/BasicDialogs example source code is now cross platform portable and both Win32 and Unix Gtk+ implementations are provided. The WinShell module has been greatly enhanced.

Fixed a bug with the compiler INT built in function. It did not accept floating-point types as a parameter.

*************** Build 4, July 28 2000 ***************
Fixed a problem in the Win32 API DEF modules on procedures that were using the Cdecl calling convention attribute. Newer compilers changed the meaning slightly and the attribute should have been changed to msCdecl. Some API procedures use this old calling convention and we were not aware any Win32 API calls used this convention.

Fixed a Peephole optimization bug that has been around for long time but only recently discovered. Its occurrence was obviously quite rare.

Fixed a linker bug linking Win32 programs that could occur when linking VC++ objects or object libraries with a massive number of sections.

Fixed a debugger problem with the execute to location feature when the location you were executing to was not in the same procedure as the current execution location.

The Ed example program did not run on Win9x. This has been fixed. It was using resource numbers > 32767 which Win9x does not support. Windows NT/2k supports resource identifiers up to 65535.

New extensions to the enumeration syntax for porting C header files to Modula-2.

The LONGCARD, CARDINAL64, type is available in 32-bit mode. This allows being technically correct when porting some operating system C header files. The compiler still performs internal arithmetic as INTEGER64 so constants and constant arithmetic is limited to the range of 64-bit integers.

*************** Build 3, July 5 2000 ***************

The problem with the "page setup" dialog not correctly initializing the page format radio button group is fixed.

The compiler directives ExportAll and PropagateExceptionAll did not apply themselves to procedure types and variables.

Fixed an environment access violation with the procedure view mode and selecting a procedure via the mouse.

Fixed the problem in the linker that occurred when using index exports from the compiler EXPORTS declaration for Win32 DLL's.

*************** Build 2, June 27 2000 ***************

The SYSTEM.SWAPENDIAN extension can now be used as a procedure as well as a function. When used as a procedure it will now accept the REAL and LONGREAL types.

Fixed an environment bug when you logged in a user name and executed a script that started a build of some sort. The script did not wait for the build to finish before executing the next script statement. If you did not change the user name no error would occur.

The Win32 debugger is now able to force its Window to the foreground on Win2k.

Fixed a bug in the 16-bit DOS Storage module which could cause command.com to not reload after the program exited.

The environment did not correctly output the PentiumII/III compiler option when automatically generating scripts.

The WINUSER.wsprintf declaration needed to be corrected.

WINX.IsThread is now imported from the SYSTEM module. This is to enhance portability to other operating systems without the need for conditional compilation. WINX.IsInit is no longer necessary and does not exist.

Fixed a Win32 RTL problem, it could not be built with the Unicode option.

*************** Build 1, May 24 2000 ***************
Initial release

