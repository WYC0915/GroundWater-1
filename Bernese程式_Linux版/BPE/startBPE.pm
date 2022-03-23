#!/usr/bin/perl -w
# -------------------------------------------------------------------------
# Bernese GPS Software Version 5.1
# -------------------------------------------------------------------------
#
# Script:     startBPE.pm
#
# Purpose:    Structure to start a BPE server
#
#             It contains all keywords of RUNBPE.INP. They are available
#             as variables. The constructor initializes all keywors
#             with default values (and may be used to redefine variables
#             from the PCF file). After that the variables may be changed
#             to the values necessary for the current BPE task. At the end
#             the subroutine "run" copies MENU.INP and RUNBPE.INP into
#             the $U/WORK directory (the processID is added), all keywords
#             are reset, the PCF variables are updated (if neccessary), the
#             BPE server runs the BPE, and the copies of the input files
#             are deleted.
#
# Example:    use lib "$ENV{'BPE'}";
#             use startBPE;
#
#             ...
#
#             # Create a new startBPE object (with default values) and
#             # (optionally) redefine PCF variables
#             # ------------------------------------------------------
#               my $BPE1 = new startBPE();
#
#               %newVar = ("V_MIX" => "YES", "V_MAXLEN" => 2500);
#               my $BPE2 = new startBPE(%newVar);
#
#             # These four keywords (variables) must be set in any case:
#             # --------------------------------------------------------
#               $$BPE{BPE_CAMPAIGN} = '${Q}/CLKFINAL';
#               $$BPE{PCF_FILE}     = 'CLKFINAL';
#               $$BPE{SESSION}      = '1240';
#               $$BPE{YEAR}         = '2003';
#
#             # Also the values for the other keywords may be changed:
#             # --------------------------------------------------------
#               $$BPE{TASKID} = "FC";
#               $$BPE{SYSOUT} = "FCLK_BPE";
#               $$BPE{STATUS} = "FCLK_BPE.SUM";
#
#             # Reset the CPU file (if it is necessary)
#             # --------------------------------------------------------
#               $BPE->resetCPU();
#
#             # Reset a special CPU file (e.g., CPU-file of the SUPERBPE)
#             # --------------------------------------------------------
#               $BPE->resetCPU($BPE{S_CPU_FILE});
#
#             # Delete BPE files from the campaign from a previous run
#             # ------------------------------------------------------
#             # (a) of this day
#               $BPE->cleanCamp();
#
#             # (b) of any other day
#               $BPE->cleanCamp("1230","2003");   # or
#               $BPE->cleanCamp("1230");          # or
#               $BPE->cleanCamp("-1");            # counted in sessions for
#                                                 # hourly sessions (A-X) or
#                                                 # counted in days!
#
#             # Delete directories from the temp. user area
#             # --------------------------------------------------------
#             # (a) of this day
#               $BPE->cleanTemp();
#
#             # (b) of any other day
#               $BPE->cleanTemp("1230","2003");   # or
#               $BPE->cleanTemp("1230");          # or
#               $BPE->cleanTemp("-1");            # counted in sessions for
#                                                 # hourly sessions (A-X) or
#                                                 # counted in days!
#
#             # The BPE server can run now:
#             # --------------------------------------------------------
#               $BPE->run();
#
#             # Exit status of the BPE server
#             # --------------------------------------------------------
#               $$BPE{ERROR_STATUS} == 0          # Successful
#               $$BPE{ERROR_STATUS} == 1          # Error
#               available from $BPE->run() and $BPE->resetCPU();
#
# Author:     R. Dach
#
# Created:    15-May-2003
#
# Changes:    22-Jul-2003 RD: Add SR to reset CPU file
#             28-Jul-2003 MM: change PCF variables implemented
#             28-Jul-2003 RD: Add campaign drive if not yet set
#             29-Jul-2003 RD: Write some status lines
#             30-Jul-2003 RD: Copy RUNBPE.INP back to $U/PAN
#             31-Jul-2003 MM: bugfix wrt PCF variables
#             14-Aug-2003 RD: Add keywords S_STATUS
#             18-Aug-2003 RD: CPU file name as an optional argument to resetCPU
#             20-Aug-2003 RD: Set "win32" variable
#             28-Aug-2003 RD: Add cleaning tools
#             16-Sep-2003 RD: Move timstr and sysprint to bpe_util.pm
#             16-Mar-2004 RD: Error handling if BPE-server failed
#             17-Mar-2004 RD: Set error code ERROR_STATUS
#             23-Mar-2004 HU: Gps_Date moved to X/EXE
#             06-Jul-2004 RD: Bugfix in getCampaign if only 1 camp. in list
#             26-Jan-2005 RD: Prints System PID of the BPE server
#             15-Mar-2005 PF/RD: Enable "\" in campaign path for WIN32
#                             Check availability of the campaign directory
#             14-Nov-2008 SL: MODULO_SESS added to _init and run
#             14-Jan-2011 SL: use different LOADGPS.setvar if available
#             16-Oct-2012 RD: Include error handling from BPE output file
#             30-Nov-2012 RD: Use Perl-modul for Windows
#
# Copyright:  Astronomical Institute
#             University of Bern
#             Switzerland
# -------------------------------------------------------------------------

package startBPE;

use Class::Struct;
use lib "$ENV{'X'}/EXE";
use Gps_Date;
use lib "$ENV{'BPE'}";
use RUNBPE;
use bpe_util;
use strict;


## Constructor
###############################################################################
sub startBPE::new {
  my $classname = shift;
  my $self = {};
  bless $self, $classname;
  unless(defined($main::win32)) {
    $main::win32 = (uc($ENV{'OS_NAME'}) =~ /^WIN/);
  }
  $self->_init(@_);
  return $self;
}

## Initialize
###############################################################################
sub startBPE::_init() {

  my $self = shift;

  $$self{ADD_SERVER_VARIABLES} = {@_};   # hash reference

#  $$self{BPE_CLIENT}           = '${BPE}/RUNBPE.sh';
  $$self{BPE_CLIENT}           = '${BPE}/RUNBPE.pm';
  if ($main::win32) {
    $$self{CLIENT_ENV}           = "";
    $$self{BPE_CLIENT}           = '${BPE}/RUNBPE.pm';
  } else {
    if(-s "$ENV{X}/EXE/LOADGPS_$ENV{HOST}.setvar") {
      $$self{CLIENT_ENV}           = "$ENV{X}/EXE/LOADGPS_$ENV{HOST}.setvar";
    } else {
      $$self{CLIENT_ENV}           = '${X}/EXE/LOADGPS.setvar';
    }
  }
  $$self{PCF_FILE}             = 'tobeset';
  $$self{CPU_FILE}             = 'CPUFILE';
  $$self{CPUUPDRATE}           = '300';
  $$self{BPE_CAMPAIGN}         = 'tobeset';
  $$self{SESSION_TABLE}        = 'SESSIONS';
  $$self{YEAR}                 = 'tobeset';
  $$self{SESSION}              = 'tobeset';
  $$self{NUM_SESS}             = '1';
  $$self{MODULO_SESS}          = '1';
  $$self{NEXTSESS}             = '0';
  $$self{SUPERBPE}             = '0';
  $$self{SCRIPT_START}         = '';
  $$self{SCRIPT_SKIP}          = '';
  $$self{TASKID}               = '00';
  $$self{SYSODEF}              = '0';
  $$self{SYSOUT}               = 'NEWBPE';
  $$self{ERRMRG}               = '1';
  $$self{SYSERR}               = 'ERROR';
  $$self{STATUS}               = 'NEWBPE.SUM';
  $$self{DEBUG}                = '0';
  $$self{NOCLEAN}              = '0';
  $$self{SERVER_VARIABLES_0}   = '';
  $$self{SERVER_VARIABLES}     = '';
  $$self{RADIO_P}              = '1';
  $$self{MAXSESS}              = '1';
  $$self{RADIO_S}              = '0';
  $$self{S_PCF_FILE}           = '';
  $$self{S_CPU_FILE}           = 'CPUFILE';
  $$self{S_CPUUPDRATE}         = '10';
  $$self{S_BPE_CAMPAIGN}       = '';
  $$self{S_SCRIPT_START}       = '';
  $$self{S_SCRIPT_SKIP}        = '';
  $$self{S_TASKID}             = 'ZZ';
  $$self{S_SYSODEF}            = '0';
  $$self{S_SYSOUT}             = 'SUPERBPE';
  $$self{S_ERRMRG}             = '1';
  $$self{S_SYSERR}             = 'ERROR';
  $$self{S_STATUS}             = 'SUPERBPE';
  $$self{S_DEBUG}              = '0';
  $$self{S_NOCLEAN}            = '0';
  $$self{S_SERVER_VARIABLES_0} = '';
  $$self{S_SERVER_VARIABLES}   = '';
  $$self{ERROR_STATUS}         = 0;
}


# Reset CPU file
###############################################################################
sub startBPE::resetCPU {
  my $self    = shift;
  my $cpuFile = shift || $$self{CPU_FILE};

  # Define the name of temporary file
  # ---------------------------------
  my $menInp = "$ENV{'U'}/WORK/MENU.INP_$$";
  my $runCPU = "$ENV{'U'}/WORK/RUNCPU.MEN_$$";

  # Copy the master of the temporary files
  # --------------------------------------
  File::Copy::copy("$ENV{'U'}/PAN/MENU.INP",  $menInp);

  # Get path and extension
  # ----------------------
  my $pthCPU = RUNBPE::getKey('',"$ENV{U}/PAN/MENU_EXT.INP","PTH_CPU");
  my $extCPU = RUNBPE::getKey('',"$ENV{U}/PAN/MENU_EXT.INP","EXT_CPU");

  # Write the "RESETCPU" command file
  # ---------------------------------
  open (RUNCPU,"> $runCPU");
  print RUNCPU "RESETCPU 1 ${pthCPU}/".$cpuFile.".${extCPU}\n\n";
  close RUNCPU;

  # Run the BPE server in non-interactive mode
  # ------------------------------------------
  if (RUNBPE::startMenu('',$menInp,$runCPU,"BPE")) {
    $$self{ERROR_STATUS} = 1;
  } else {
    $$self{ERROR_STATUS} = 0;
  }

  # Remove temporary files
  # ----------------------
  if ($$self{NOCLEAN} eq "0") {
    unlink($menInp,$runCPU);
  }
}


# Clean up campaign
###############################################################################
sub startBPE::cleanCamp {
  my $self  = shift;
  my $dSess = shift || $$self{SESSION};
  my $dYear = shift || $$self{YEAR};

  # Compute session if necessary
  if (substr($dSess,0,1) eq "-") {
    if ($$self{SESSION} =~ /[A-X]$/) {
      $dYear = gps_date("-yd",$$self{YEAR},$$self{SESSION},
                        "-h",$dSess,"-o","%y");
      $dSess = gps_date("-yd",$$self{YEAR},$$self{SESSION},
                        "-h",$dSess,"-o","%j%I");
    } else {
      $dYear = gps_date("-yd",$$self{YEAR},$$self{SESSION},
                        "-d",$dSess,"-o","%y");
      $dSess = gps_date("-yd",$$self{YEAR},$$self{SESSION},
                        "-d",$dSess,"-o","%j").substr($$self{SESSION},3,1);
    }
  } else {
    $dYear = gps_date("-yd",$dYear,$dSess,"-o","%y");
  }

  # Get path and extensions
  my $camp   = RUNBPE::_expandEnv('',$self->getCampaign());

  if (-d $camp) {

    my $dirPrt = RUNBPE::getKey('',"$ENV{U}/PAN/MENU_EXT.INP","DIR_BPEPRT");
    my $extPrt = RUNBPE::getKey('',"$ENV{U}/PAN/MENU_EXT.INP","EXT_BPEPRT");

    my $dirLog = RUNBPE::getKey('',"$ENV{U}/PAN/MENU_EXT.INP","DIR_BPELOG");
    my $extLog = RUNBPE::getKey('',"$ENV{U}/PAN/MENU_EXT.INP","EXT_BPELOG");

    unlink(glob("${camp}/${dirPrt}/$$self{TASKID}${dYear}${dSess}_???_???.${extPrt}"));
    unlink(glob("${camp}/${dirLog}/$$self{TASKID}${dYear}${dSess}_???_???.${extLog}"));
  }
}



# Clean up temporary environmant
###############################################################################
sub startBPE::cleanTemp {
  my $self  = shift;
  my $dSess = shift || $$self{SESSION};
  my $dYear = shift || $$self{YEAR};

  # Compute session if necessary
  if (substr($dSess,0,1) eq "-") {
    if ($$self{SESSION} =~ /[A-X]$/) {
      $dYear = gps_date("-yd",$$self{YEAR},$$self{SESSION},
                        "-h",$dSess,"-o","%y");
      $dSess = gps_date("-yd",$$self{YEAR},$$self{SESSION},
                        "-h",$dSess,"-o","%j%I");
    } else {
      $dYear = gps_date("-yd",$$self{YEAR},$$self{SESSION},
                        "-d",$dSess,"-o","%y");
      $dSess = gps_date("-yd",$$self{YEAR},$$self{SESSION},
                        "-d",$dSess,"-o","%j").substr($$self{SESSION},3,1);
    }
  } else {
    $dYear = gps_date("-yd",$dYear,$dSess,"-o","%y");
  }

  my @dirLst = glob("$ENV{T}/BPE_$$self{PCF_FILE}_*_${dYear}_${dSess}_???_???");

  foreach my $tmpDir (@dirLst) {
    if (-d $tmpDir) {
      File::Path::rmtree($tmpDir)
    }
  }
}


# Run the BPE server (menu)
###############################################################################
sub startBPE::run {
  my $self = shift;

  # Check that the minimum of the keywords are defined by the user
  # --------------------------------------------------------------
  if ($$self{SESSION}  eq 'tobeset' || $$self{YEAR}         eq 'tobeset' ||
      $$self{PCF_FILE} eq 'tobeset' || $$self{BPE_CAMPAIGN} eq 'tobeset') {
    warn(" *** startBPE: At least the values for \n".
       "         BPE_CAMPAIGN, SESSION, YEAR, and PCF_FILE\n".
       "         have to be set");
    exit(1);
  }

  # Define the name of temporary file
  # ---------------------------------
  my $menInp = "$ENV{'U'}/WORK/MENU.INP_$$";
  my $bpeInp = "$ENV{'U'}/WORK/RUNBPE.INP_$$";
  my $runbpe = "$ENV{'U'}/WORK/RUNBPE.MEN_$$";
  my $setVar = "$ENV{'U'}/WORK/SETVAR.MEN_$$";

  # Copy the master of the temporary files
  # --------------------------------------
  File::Copy::copy("$ENV{'U'}/PAN/MENU.INP",  $menInp);
  if (-s "$ENV{'U'}/PAN/RUNBPE.INP") {
    File::Copy::copy("$ENV{'U'}/PAN/RUNBPE.INP",  $bpeInp);
  } else {
    File::Copy::copy("$ENV{'X'}/PAN/RUNBPE.INP",  $bpeInp);
  }

  # Set the time variables for the current session
  # ----------------------------------------------
  my $MJD = gps_date("-yd",$$self{YEAR},substr($$self{SESSION},0,3),"-o","%J");

  # Find campaign in MENU_CMP.INP
  # -----------------------------
  $$self{BPE_CAMPAIGN} = $self->getCampaign($menInp);

  # Reset keywords in MENU.INP
  # --------------------------
  RUNBPE::putKey('',$menInp,"ACTIVE_CAMPAIGN",$$self{BPE_CAMPAIGN});
  RUNBPE::putKey('',$menInp,"SESSION_TABLE",$$self{SESSION_TABLE});
  RUNBPE::putKey('',$menInp,"SESSION_CHAR",substr($$self{SESSION},3,1));
  RUNBPE::putKey('',$menInp,"MODJULDATE",$MJD);
  RUNBPE::putKey('',$menInp,"JOB_ID","");

  # Reset keywords in RUNBPE.INP
  # ----------------------------
  RUNBPE::putKey('',$bpeInp,"BPE_CLIENT",          $$self{BPE_CLIENT});
  RUNBPE::putKey('',$bpeInp,"CLIENT_ENV",          $$self{CLIENT_ENV});
  RUNBPE::putKey('',$bpeInp,"PCF_FILE",            $$self{PCF_FILE});
  RUNBPE::putKey('',$bpeInp,"CPU_FILE",            $$self{CPU_FILE});
  RUNBPE::putKey('',$bpeInp,"CPUUPDRATE",          $$self{CPUUPDRATE});
  RUNBPE::putKey('',$bpeInp,"NUM_SESS",            $$self{NUM_SESS});
  RUNBPE::putKey('',$bpeInp,"MODULO_SESS",         $$self{MODULO_SESS});
  RUNBPE::putKey('',$bpeInp,"NEXTSESS",            $$self{NEXTSESS});
  RUNBPE::putKey('',$bpeInp,"SUPERBPE",            $$self{SUPERBPE});
  RUNBPE::putKey('',$bpeInp,"SCRIPT_START",        $$self{SCRIPT_START});
  RUNBPE::putKey('',$bpeInp,"SCRIPT_SKIP",         $$self{SCRIPT_SKIP});
  RUNBPE::putKey('',$bpeInp,"TASKID",              $$self{TASKID});
  RUNBPE::putKey('',$bpeInp,"SYSODEF",             $$self{SYSODEF});
  RUNBPE::putKey('',$bpeInp,"SYSOUT",              $$self{SYSOUT});
  RUNBPE::putKey('',$bpeInp,"ERRMRG",              $$self{ERRMRG});
  RUNBPE::putKey('',$bpeInp,"SYSERR",              $$self{SYSERR});
  RUNBPE::putKey('',$bpeInp,"STATUS",              $$self{STATUS});
  RUNBPE::putKey('',$bpeInp,"DEBUG",               $$self{DEBUG});
  RUNBPE::putKey('',$bpeInp,"NOCLEAN",             $$self{NOCLEAN});
  RUNBPE::putKey('',$bpeInp,"SERVER_VARIABLES_0",  "");
  RUNBPE::putKey('',$bpeInp,"SERVER_VARIABLES",    "");
  RUNBPE::putKey('',$bpeInp,"RADIO_P",             $$self{RADIO_P});
  RUNBPE::putKey('',$bpeInp,"MAXSESS",             $$self{MAXSESS});
  RUNBPE::putKey('',$bpeInp,"RADIO_S",             $$self{RADIO_S});
  RUNBPE::putKey('',$bpeInp,"S_PCF_FILE",          $$self{S_PCF_FILE});
  RUNBPE::putKey('',$bpeInp,"S_CPU_FILE",          $$self{S_CPU_FILE});
  RUNBPE::putKey('',$bpeInp,"S_CPUUPDRATE",        $$self{S_CPUUPDRATE});
  RUNBPE::putKey('',$bpeInp,"S_BPE_CAMPAIGN",      $$self{S_BPE_CAMPAIGN});
  RUNBPE::putKey('',$bpeInp,"S_SCRIPT_START",      $$self{S_SCRIPT_START});
  RUNBPE::putKey('',$bpeInp,"S_SCRIPT_SKIP",       $$self{S_SCRIPT_SKIP});
  RUNBPE::putKey('',$bpeInp,"S_TASKID",            $$self{S_TASKID});
  RUNBPE::putKey('',$bpeInp,"S_SYSODEF",           $$self{S_SYSODEF});
  RUNBPE::putKey('',$bpeInp,"S_SYSOUT",            $$self{S_SYSOUT});
  RUNBPE::putKey('',$bpeInp,"S_ERRMRG",            $$self{S_ERRMRG});
  RUNBPE::putKey('',$bpeInp,"S_SYSERR",            $$self{S_SYSERR});
  RUNBPE::putKey('',$bpeInp,"S_STATUS",            $$self{S_STATUS});
  RUNBPE::putKey('',$bpeInp,"S_DEBUG",             $$self{S_DEBUG});
  RUNBPE::putKey('',$bpeInp,"S_NOCLEAN",           $$self{S_NOCLEAN});
  RUNBPE::putKey('',$bpeInp,"S_SERVER_VARIABLES_0",$$self{S_SERVER_VARIABLES_0});
  RUNBPE::putKey('',$bpeInp,"S_SERVER_VARIABLES",  $$self{S_SERVER_VARIABLES});


  # Run menu to expand variables
  # ----------------------------
  open (SETVAR,"> $setVar");
  print SETVAR "INP_FILE_NAME 1 $bpeInp\n\n";
  print SETVAR "OUT_FILE_NAME 1 $bpeInp\n\n";
  close SETVAR;
  RUNBPE::startMenu('',$menInp,$setVar);

  # Append/prepend new user variables
  # ---------------------------------
  if (keys(%{$$self{ADD_SERVER_VARIABLES}})) {
    open (SEL,"> $setVar");
    my ($key, $value);
    while ( ($key, $value) = each %{$$self{ADD_SERVER_VARIABLES}}) {
      print SEL "\"$key\" \"$value\" \"redefined\"\n";
    }
    close SEL;
    RUNBPE::putKey("",$bpeInp,"SERVER_VARIABLES","","PREPEND",$setVar);
    RUNBPE::putKey("",$bpeInp,"SERVER_VARIABLES","","APPEND",$setVar) }

  # Copy RUNBPE.INP back to $U/PAN
  # ------------------------------
  File::Copy::copy($bpeInp,"$ENV{'U'}/PAN/RUNBPE.INP");

  # Write the "RUNBPE" command file
  # -------------------------------
  open (RUNBPE,"> $runbpe");
  print RUNBPE "RUN_BPE 1 $bpeInp\n\n";
  print RUNBPE "PRINT_PID 1  \"1\"\n\n" unless ($main::win32);
  close RUNBPE;

  # Write a starting message
  # ------------------------
  my $pcfName = RUNBPE::getKey('',$bpeInp,"PCF_FILE");
  my $cpuName = RUNBPE::getKey('',$bpeInp,"CPU_FILE");
  my $cmpName = RUNBPE::getKey('',$bpeInp,"BPE_CAMPAIGN");
  my $sesName = RUNBPE::getKey('',$bpeInp,"SESSION");
  my $yr4Name = RUNBPE::getKey('',$bpeInp,"YEAR");
  my $outName = RUNBPE::getKey('',$bpeInp,"SYSOUT");
  my $sumName = RUNBPE::getKey('',$bpeInp,"STATUS");

  sysprint ("\n");
  sysprint ("Starting BPE on ".timstr(localtime(time))."\n");
  sysprint ("------------------------------------\n\n");
  sysprint ("PCFile:         $pcfName\n");
  sysprint ("CPU file:       $cpuName\n");
  sysprint ("Campaign:       $cmpName\n");
  sysprint ("Year/session:   $yr4Name/$sesName\n");
  sysprint ("BPE output:     $outName\n");
  sysprint ("BPE status:     $sumName\n\n");

  # Is the campaign directory available?
  # ------------------------------------
  unless (-d RUNBPE::_expandEnv('',$$self{BPE_CAMPAIGN})) {
    sysprint ("The campaign directory \"$$self{BPE_CAMPAIGN}\" does not exist.\n\n");
    sysprint ("BPE-error at ".timstr(localtime(time))."\n");
    sysprint ("---------------------------------\n\n");
    $$self{ERROR_STATUS} = 1;

  } else {

  sysprint ("BPE server runs ");

  # Run the BPE server in non-interactive mode
  # ------------------------------------------
  if ( RUNBPE::startMenu('',$menInp,$runbpe,"BPE")) {
    sysprint ("\n");
    sysprint ("BPE error at ".timstr(localtime(time))."\n");
    sysprint ("---------------------------------\n\n");
    $$self{ERROR_STATUS} = 1;
  } else {
    
    # Check the program output for errors
    my $outFil = RUNBPE::_expandEnv('',$outName);
    unless ( -e $outFil ) {
      sysprint ("The BPE output is missing: $outName\n\n");
      sysprint ("BPE error at ".timstr(localtime(time))."\n");
      sysprint ("---------------------------------\n\n");
      $$self{ERROR_STATUS} = 1;
    } else {
      my $iErr = 2;
      open(OUT,"$outFil");
      while(<OUT>) {
        if ( /Sessions finished:/ ) {
          $iErr = /Error: 0/ ? 0 : 1;
          last;
        }
      }
      close OUT;
      if ( $iErr == 0 ) {
        sysprint ("\n");
        sysprint ("BPE finished at ".timstr(localtime(time))."\n");
        sysprint ("------------------------------------\n\n");
        $$self{ERROR_STATUS} = 0;
      } elsif ( $iErr == 1 ) {
        sysprint ("\n");
        sysprint ("User script error in: $outName\n\n");
        sysprint ("BPE error at ".timstr(localtime(time))."\n");
        sysprint ("---------------------------------\n\n");
        $$self{ERROR_STATUS} = 1;
      } else {
        sysprint ("\n");
        sysprint ("Incomplete BPE output file: $outName\n\n");
        sysprint ("BPE error at ".timstr(localtime(time))."\n");
        sysprint ("---------------------------------\n\n");
        $$self{ERROR_STATUS} = 1;
    } }
  } }

  # Remove temporary files
  # ----------------------
  if ($$self{NOCLEAN} eq "0") {
    unlink($menInp,$bpeInp,$setVar,$runbpe);
  }
}


sub startBPE::getCampaign{

  my $self   = shift;
  my $menInp = shift || "$ENV{'U'}/PAN/MENU.INP";;
  my $camp   = shift || $$self{BPE_CAMPAIGN};

  # Find campaign in MENU_CMP.INP
  # -----------------------------
  # Get name of campaign list

  my $campFil = RUNBPE::getKey('',$menInp,"MENU_CMP_INP");
  $campFil  = RUNBPE::_expandEnv('',$campFil);

  # Search for string '/cmp_name"' in campaign list
  #  (as a part of the string '   "${X}/cmp_name"')
  my $toBeFound = $camp . "\"";

  open (CMP,$campFil);
  while (my $lin=<CMP>) {

    $lin =~ s/^CAMPAIGN[ ]*1 //;

    if ($lin =~ /[\/\\]$toBeFound/ ) {  # searching for
                                        # '/CAMP"' or '\CAMP"'

      $lin =~ s/\"//g;                  # Remove quotes
      $lin =~ s/ //g;                   # Remove blanks
      chomp($lin);                      # Remove line feed
      $camp = $lin;
    }
  }
  close (CMP);

  return $camp;
}

1;

__END__
