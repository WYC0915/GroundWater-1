====================================================================
Bernese GNSS Software, Version 5.2            Last mod.: 31-Oct-2012
--------------------------------------------------------------------
Update procedure
====================================================================


--------------------------------------------------------------------
Important remark
--------------------------------------------------------------------
  Please be careful if you made some modifications in the software!
  If this is the case, you should extract the update tar-file into
  a temporary directory different to the default directory and merge
  your modifications with the updated files.


--------------------------------------------------------------------
Update procedure with the update utility
--------------------------------------------------------------------
To update your release version, please proceed as follows:

. Start the Perl script "configure.pm" in ${X}/EXE (UNIX) or
  "perl %X%\EXE\configure.pm" for Windows from a command line and
  select the step "2) Install online updates".

  The utility will check your installation for completeness and will
  find your current release version.


. If the program "wget" is installed on your operating system (which
  is normally the case for UNIX/Linux), the update utility looks
  automatically for an online update for your release version.
  The corresponding archive will be downloaded and you will be asked
  if the files should really be extracted. If yes, the update is done
  automatically.

  If "wget" is not available, you will be asked to download the
  appropriate update archive together with access information to the
  website where these files are located.
  Then you can download the update file and save it in the Bernese top
  directory $C (UNIX) or %C% (Windows), respectively.

  Windows:
    The update utility contains several options for an extraction of
    the ZIP-archives from the command line. If one of these options
    is available, the utility will extract the files to a directory
    of your choice (default: the Bernese top directory).
    If none of the supported tools is installed on your computer, the
    update utility asks you to extract the files manually by using
    winzip or any other unzip utility.


. From the extraction of the archives only the files in the main tree
  of the Bernese GNSS Software (${C} or %C%) are updated. If a
  program input panel has changed, you also have to update the input
  files in the users' environments (ALL USERS!!). If this is the
  case, the utility starts the Bernese menu with the panel of the
  program UPDPAN. Fill the directories (${U}/PAN; ${U}/OPT/* or a
  list of directories from an UPD-file) and run the program. After
  updating all program input panels in the users' directories you may
  quit the Bernese menu. For more information on the UPDPAN program,
  please refer to the online help.


The update of the source and supporting files is now complete.
All users on UNIX/Linux platforms must recompile the source code in
order to make the update active. The same applies to users on Windows
platforms using their own compiler (and not the distributed binaries,
see below). This is achieved with the menu item "5) Compile the
programs" in the configure.pm script.

Windows users without a compiler can download the executables from
the most recent release of the Bernese GNSS Software together with
the update archive and extract it into %XG%-directory (default:
%C%\PGM\EXE_AIUB).


--------------------------------------------------------------------
Updating for individual bugs/corrections/improvements
--------------------------------------------------------------------
We will publish from time to time bug-fixes, corrections, or
improvements on the web-page

  http://www.bernese.unibe.ch/support

(select your release). You find there instructions on how to
download the affected files, where to place them in your environment,
and how to recompile the source code.

Note: these updates are cumulative! You have to consider all of
them. It may damage your installation if you try to establish only
selected bug-fixes, corrections, or improvements.

The release date of your installation can easily be checked in the
menu "Help > About".


--------------------------------------------------------------------
Closing remarks
--------------------------------------------------------------------
- Keep your source code, executables, and supporting files
  consistent at all times. Remember that program input files are
  also located in the user environment in the home directory of EACH
  individual user of your installation.

- We strongly recommend to update your version as soon as a new
  release has been published, or a new bug-fix, correction, or
  improvement is announced on the web-page.

- A release contains all indiviually listed bug-fixes, corrections,
  and improvements.

- To all users of version 5.0:
  The update package for your release contain now all changes and
  brings you directly to the latest release. In contrast to version
  5.0 you have only to install one update package to come on the
  latest release level.


======================================================================
