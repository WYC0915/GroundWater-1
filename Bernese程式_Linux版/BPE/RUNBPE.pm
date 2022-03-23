#!/usr/bin/perl -w
# -------------------------------------------------------------------------
# Bernese GPS Software Version 5.1
# -------------------------------------------------------------------------
#
# Script:     RUNBPE.pm
#
# Purpose:    This is the first BPE script called directly from the
#             BPE server (e.g. using the ssh or rsh command). It
#             opens the tcp connection to the server, prepares the
#             environment and then starts the user script that
#             resides in $U/SCRIPT directory. After the user script
#             is finished RUNBPE.pm reports it to the server, close
#             the connection and exits.
#
#             This script also can be used directly as a perl module
#             from the client machine:
#             use RUNBPE;
#             my $bpe = RUNBPE->new(SCRIPT => 'scriptname', VAR1 => VAL1, ...);
#             $bpe->copyUarea;
#             $bpe->run;
#
# Envir.:     No environment variables need be set before running this script.
#
# Author:     L.Mervart, D.Hunt
#
# Created:    16-Dec-2000
#
# Changes:    23-Aug-2010 SL: perl 5.10 bugfix from 15-Feb-2008 (D.Hunt) added
#             03-Jan-2011 SL: use CMD_PATHc on last reRun (sub reRun)
#             03-Jan-2012 SL/RD: print time if debug in startMenu
#
# Copyright:  Astronomical Institute
#              University of Bern
#                  Switzerland
# -------------------------------------------------------------------------

return 1 if (defined caller); # If this is loaded as a perl module,
                              # don't execute main

use strict;
use Config;
use IO::File;
use IO::Socket;
use File::Copy;
use File::Path;
use File::Basename;
use Time::Local;
use Getopt::Long;
use Time::HiRes;

# Global Variables
# ----------------
$main::win32 = ($Config{'osname'} =~ /MSWin32/);

my ($server, $serverHostName, $port, $PID, $SUB_PID, $setVarString);

# (Re-)Connect
##############################################################################
sub reconnect {
use Sys::Hostname;

  my $debug = 0;

  if (!defined($server) || !$server->connected()) {
    my $retries = 1500;
    my $wait    = ($SUB_PID+1)/1000;
    while ($retries--) {
      $server = IO::Socket::INET->new( Proto    => "tcp",
                                       PeerAddr => $serverHostName,
                                       PeerPort => $port);
      last if ($server);

      select(undef, undef, undef, $wait);
    }
    die "Cannot connect to $serverHostName on $port: $!" unless ($server);
    print hostname.": client connected to the server: PID=$PID && SUB_PID=$SUB_PID\n" if $debug;
    print $server "MESSAGE=CONNECTED && PID=$PID && SUB_PID=$SUB_PID\n";
    print hostname.": client sent start message: PID=$PID && SUB_PID=$SUB_PID\n" if $debug;
  }
}

# main program
##############################################################################
{
  # Command Line Options
  # --------------------
  my $infile = '';
  my $stdout = '';
  my $stderr = '';
  GetOptions("infile=s" => \$infile,
             "stdout=s" => \$stdout,
             "stderr=s" => \$stderr);

  # List of Parameters
  # ------------------
  ($serverHostName, $port, $PID, $SUB_PID) = @ARGV;
  if (!defined($SUB_PID)) {
    die "Usage: RUNBPE.pm serverHostName portNumber PID SUB_PID\n";
  }

  # Re-direct the standard output (useful under MSWin32)
  # ----------------------------------------------------
  if ($stdout) {
    CORE::close(STDOUT);
    open(STDOUT, ">> $stdout");
  }

  # Re-direct the standard error (useful under MSWin32)
  # ----------------------------------------------------
  if ($stderr) {
    CORE::close(STDERR);
    if ($stderr eq $stdout) {
      open(STDERR, ">>& STDOUT");
    } else {
      open(STDERR, ">> $stderr");
    }
  }

  # Local Variables
  # ---------------
  my ($noclean, $inMsg, $ctrFile, $tmpUserArea,
      $debugFile, $irc, $prtFile, $isUbelix, $uwork);

  # Open the Input File or Connect to the Server
  # --------------------------------------------
  if ($infile) {
    $server = IO::File->new($infile);
  } else {
    reconnect();
  }

  # Listen to the Server
  # --------------------
  MESSAGE:
  while ($inMsg = <$server>) {

    my $obj = eval { RUNBPE->new($inMsg) }; # create new command object
    if ($@) {
      warn "script compile error:\n $@";
      my $outMsg = "MESSAGE=FINISHED && PID=$PID && SUB_PID=$SUB_PID"
                 . " && STATUS=1";
      reconnect(); print $server "$outMsg\n" unless ($infile);
      next MESSAGE;
    }

    my $command = $obj->command;

    # Command START
    # -------------
    if    ($command eq 'START') {
      $$obj{START_TIME} = Time::HiRes::time(); # remember start time
      $irc = 0;
      $noclean = $obj->noclean;
      eval {
        $prtFile = $obj->prtFile;                        # name of protocol
        $tmpUserArea = $obj->copyUarea->tmpDir;          # create working area
        $obj->initTimeVar;                               # Time variables
        $obj->setParallelParams;                         # master and slave
        $ctrFile = $obj->ctrFile;                        # control file
        $obj->writePrtFileHeader if (defined($prtFile)); # protocol header
        $obj->writeLogFileHeader;
        $irc = $obj->reRun;                              # run BPE script
        $isUbelix = defined $$obj{BPE_SERVER_HOST} &&
                    $$obj{BPE_SERVER_HOST} eq "sedna";
      };
      if ($@) { warn $@; $irc = 1; }

      $uwork = $$obj{U_OLD}."/WORK/";
      chdir($uwork);

      $$obj{FINISHED_TIME} = Time::HiRes::time(); # remember finished time

      my $outMsg = "MESSAGE=FINISHED && PID=$PID && SUB_PID=$SUB_PID"
                 . " && STATUS=$irc";
      if (defined($setVarString)) {
          $outMsg .= " && SETVAR=$setVarString";
      }
      if (defined($prtFile)) {
#        $outMsg .= " && PRT_FILE_CONTENT=" . fileToMsg($prtFile);
        $outMsg .= " && PRT_FILE_CONTENT=" . $obj->goto;
      }
      if (defined($ctrFile)) {
        $outMsg .= " && CONTROL_FILE_CONTENT=" . fileToMsg($ctrFile);
      }
      $outMsg .= " && RERUN_COUNT=$$obj{RERUN_COUNT}";
      $outMsg .= " && TIME_SCRIPT=" . $obj->timeScript();
      $outMsg .= " && TIME_PGM=" . $obj->timePgm();
      reconnect(); print $server "$outMsg\n" unless ($infile);
    }

    # Command QUIT
    # ------------
    elsif ($command eq 'QUIT') {
      if (!$noclean && $irc == 0) {
        File::Path::rmtree($tmpUserArea);  # clean up temp user area
      } elsif ( $isUbelix ) {
        my $copyUserArea = $uwork . basename $tmpUserArea;
        copyUserTree($tmpUserArea,$copyUserArea);
      }
      CORE::close $server;
      RUNBPE::writePrtFileTail($prtFile) if (defined($prtFile));
      exit;
    }

    $obj->close;
  }
}

# Read the content of a file into a message string
##############################################################################
sub fileToMsg {

  # List of Parameters
  # ------------------
  my ($fileName) = @_;

  if (! -s $fileName) { return ""; }

  # Local Variables
  # ---------------
  my ($outMsg) = "";
  my($line);

  open(inFile,  "<$fileName") || die "Cannot open file $fileName";
  while ( defined($line=<inFile>) ) {
    $line =~ s/[[:^print:]]/ /g; # replace non-printable characters by blanks
    chop($line);
    $outMsg = $outMsg . $line . " & ";
  }
  chop($outMsg);
  CORE::close(inFile);

  return $outMsg;
}

# Make a copy of the temporary user environment
##############################################################################
sub copyUserTree {
  my $fromDir = shift;
  my $copyDir = shift;

  # Read the directory recursive
  # ----------------------------
  my @dirToRead = ();
  push @dirToRead, $fromDir;

  # Read the next directory from list
  # ---------------------------------
  while (@dirToRead) {
    my $dirName = shift @dirToRead;

    # Target directory name
    my $dirNam2 = $dirName;
    $dirNam2 =~ s/^$fromDir/$copyDir/;
    mkpath($dirNam2) unless -d $dirNam2;

    # Read all files in the current directory
    opendir(FROM,"$dirName");
    my @thisDir = readdir(FROM);
    closedir(FROM);

    # Each file in the directory
    foreach my $thisFile (@thisDir) {
      if ($thisFile eq "." || $thisFile eq "..") { next }

      # A directory found
      if (-d "$dirName/$thisFile") {
        push @dirToRead, "$dirName/$thisFile";
        next
      }

      # From and To
      my $from = "$dirName/$thisFile";
      my $to   = "$dirNam2/$thisFile";

      copy($from , $to) || die ("cannot copy $from to $to");
    }
  }

}

# -------------------------------------------------------------------------
# Bernese GPS Software Version 5.0
# -------------------------------------------------------------------------
#
# Package:    RUNBPE
#
# Purpose:    This package implements an object which is used to handle one
#             BPE client request. It stores all variables from the input
#             message into object variables. It has methods for querying the
#             object, managing protocol files, setting and querying keys
#             in .INP files and running the requested Bernese script.
#
# Constructor - subroutine new:
#
# Create a new BPEclient object from the socket message read from the server.
# Adds all values from the input message to the object.  Also expands values
# from the CLIENT_ENV file, if specified in the message. In the case a perl
# SCRIPT is specified (the SCRIPT variable passed in ends in .PM) then this
# file is dynamically loaded, over-riding the default 'run' method for
# BPEclient.
#
# This method can either be invoked with an input message of the form:
#
#      VARIABLE1=VALUE1 && VARIABLE2=VALUE2 ...
#
# or with a perl hash:
#
#     (VARIABLE1 => 'VALUE1', VARIABLE2 => 'VALUE2', ...
#
# This latter invocation is useful when calling RUNBPE directly from
# a perl program on the client.
#
# These values MUST be defined in the input message or in the CLIENT_ENV
# file from the input message:
#
#   SCRIPT   -- The name of the BPE script to run.  May be a shell script
#               or (if it ends in .PM) a perl module.
#   U        -- The user directory which contains the scripts (in U/SCRIPT)
#   T        -- The temporary directory for BPE use
#   XQ       -- The location of the 'menu' executable
#   XG       -- The Bernese PGM executable directory (contains PUTKEYW and
#               GETKEY)
#   BPE      -- The location of the BPE source directory.
#   YEAR     -- The four digit year.  Used in creating protocol files.
#   SESSION  -- The four character session ID.  Used in creating protocol
#               files, as input to the 'menu' program, and in BPE scripts.
#   MJD      -- The modified julian day.  Used as input to the 'menu' program.
#   CAMP_DRV -- Campaign directory variable.  Contains the variable
#               abbreviation used for the campaign directory.  Ex: ${P}/
#               In addition, the variable specified in CAMP_DRV must be passed
#               in. So, in this case P must also be defined as the full path to
#               the campaign directory.
#   CAMP_PTH -- The full path of the campaign directory.
#   CAMPAIGN -- The name of the campaign.  Used in the 'menu' call and by
#               BPE scripts.
#   OPTDIR   -- The name of the directory in which PCF-specific .INP files
#               are stored.  Used in setting up the temporory directory.
#   TASKID   -- The task id for super BPE.  Used to name the protocol file.
#   PID      -- The PID of this BPE request.  Used to name the protocol file.
#   SUB_PID  -- The SUB_PID of this BPE request (for parallel BPE jobs).
#               Used to name the protocol file.
#   PCFFIL   -- The name of the PCF file used.  Used to name the temp area.
#
# The following values are optional:
#
#   DEBUG    -- If this is true, then set things up for better debugging.
#   TIMEOUT  -- Used to specify a max time in seconds for the FORTRAN program
#               to run.  Jobs which run longer than this will be killed and
#               an error returned.  The default (if no TIMEOUT specified) is
#               to let FORTRAN programs run forever.
#
# Parameter:  type     Type of object (normally 'BPEclient' unless subclassed)
#             message  Message string sent from server.  The format is:
#                      KEYWORD1=VALUE1 && KEYWORD2=VALUE2 ...
#
# Return:    A blessed RUNBPE object
#
# Example:
#   my $obj = RUNBPE->new ('VARIABLE1=VALUE1 && VARIABLE2=VALUE2 && ...');
#   my $obj = RUNBPE->new (VARIABLE1 => 'VALUE1', VARIABLE2 => 'VALUE2', ...);
#
# Author:     D. Hunt, L. Mervart
#
# Created:    12-JUL-2002
#
# -------------------------------------------------------------------------
package RUNBPE;
use Sys::Hostname;
use Time::Local;
use vars qw /$pid/; # use the $pid variable globally (e.g. in RUN_PGMS)

# Constructor
##############################################################################
sub RUNBPE::new {
  my $type   = shift;
  my $msg;
  my $self; # hashref for object (holds ENV variables and message variables)
  if (@_ == 1) {
    $msg  = shift;
    $self = {}; # Empty object--a hash reference
    _loadMsg ($self, $msg);
  } else { # values passed in a hash
    $self = {@_};  # take a reference to the input hash
  }

  # Handle QUIT Case
  # ----------------
  return bless $self, $type unless (defined ($$self{SCRIPT}));

  # Now load in the code contained in $self->{SCRIPT}, if $self->{SCRIPT}
  # is a perl module (ends in .PM).  If the script is a perl module,
  # bless $self as this type of object.  The script must define the 'run'
  # routine correctly, overriding the built-in 'run' method in RUNBPE.pm,
  # which is used for shell BPE scripts.
  # ---------------------------------------------------------------------
  my $script = $$self{SCRIPT} || '';
  my $dir;
  if (defined $$self{S_PTH_SCRIPT}) {
    $dir = $$self{S_PTH_SCRIPT};
  }
  else {
    $dir = "$$self{U}/SCRIPT";
  }
  open (SCR, "$dir/$script") || die "Cannot open $dir/$script";
  my $line;
  while ($line = <SCR>) { last unless (($line =~ /^\s*\#/) ||
                                       ($line =~ /^\s*$/)); }
  CORE::close SCR;
  if ($line =~ /package\s+(\w+)\;/) {
    my $pkg    = $1;
    my $symtab = "$pkg\:\:";  # the name of the symbol table for the package
    no strict 'refs'; # perl 5.10 now catching this--must turn it off!
                      # D. Hunt 2/15/2008
    unless (%{$symtab}) { require "$dir/$script"; }
    use strict 'refs';
    bless $self, $pkg;    # now a member of this class--'run' script overloaded
  } else {
    bless $self, $type;   # a generic BPEclient object
  }
  return $self;
}

# Destructor
##############################################################################
sub RUNBPE::close {
  my $self   = shift;
  undef %$self;
}

# Compute and return the name of the protocol file.
##############################################################################
sub RUNBPE::prtFile {
  my $self = shift;

  my $fileName = $$self{TASKID} . $$self{YEAR} . $$self{SESSION} . "_" .
                 $$self{PID} . "_" . $$self{SUB_PID};

  $$self{PRT_FILE} = $$self{PTH_BPEPRT} . $fileName . "." . $$self{EXT_BPEPRT};
  $$self{LOG_FILE} = $$self{PTH_BPELOG} . $fileName . "." . $$self{EXT_BPELOG};

  return $$self{PRT_FILE};
}

# Return the name of the control file
##############################################################################
sub RUNBPE::ctrFile {
  my $self = shift;
  return $$self{'CONTROL_FILE'};
}

# Return a GOTO address
##############################################################################
sub RUNBPE::goto {
  my $self = shift;
  if (defined $$self{'GOTO_PID'}) {
    return $$self{'GOTO_PID'};
  } else {
    return "";
  }
}

# Create a command for setting a variable on server (blanks are not allowed)
##############################################################################
sub RUNBPE::setVar {
  my $self      = shift;
  my $varName   = shift; # variable name
  my $varValue  = shift; # (new) variable value
  if (defined($setVarString)) {
      $setVarString .= " ";
  }
  $setVarString .= "\"" . $varName . "\" \"" . $varValue . "\"";
}

# Create a new temporary user
##############################################################################
sub RUNBPE::copyUarea {
  my $self   = shift;

  # Modify  U and T
  # ---------------
  my $pcffil    = $$self{PCFFIL};
  if ( $pcffil =~ /([^\.]*)(\.)(.*)/) {
    $pcffil = $1;
  }
  my $port      = $$self{PORT};
  my $year      = $$self{YEAR};
  my $session   = $$self{SESSION};
  my $T_old     = $$self{T};
  my $U_new     =
    "$T_old/BPE_$pcffil\_$port\_$year\_$session\_$$self{PID}\_$$self{SUB_PID}";
  my $U_old     = $$self{U};
  $$self{U}     = $U_new;
  $$self{U_OLD} = $U_old;
  $$self{T_OLD} = $T_old;

  # The T_new is not created under windows as the ':' character is not allowed
  # --------------------------------------------------------------------------
  unless ($main::win32) {
    my $T_new = "$U_new/WORK/T:";
    $$self{T} = $T_new;
    File::Path::mkpath("$T_new");
    File::Path::mkpath("$T_new/AUTO_TMP");
    unless ( -d "$T_new/AUTO_TMP" ) {
      die "Error creating $T_new directory";
    }
  } else {
    unless (-d "$$self{T}/AUTO_TMP") {
      File::Path::mkpath("$$self{T}/AUTO_TMP");
    }
    unless ( -d "$$self{T}/AUTO_TMP" ) {
      die "Error creating $$self{T}/AUTO_TMP directory";
    }
  }

  # Create the new User Area and copy all Input Files
  # -------------------------------------------------
  File::Path::mkpath("$U_new");
  File::Path::mkpath("$U_new/INP");
  File::Path::mkpath("$U_new/PAN");
  File::Path::mkpath("$U_new/WORK");

  unless ( -d "$U_new/INP"      &&
           -d "$U_new/PAN"      &&
           -d "$U_new/WORK"     ) {
    die "Error creating $U_new directory structure";
  }

  # Copy .INP-files from OPT-dir
  # ----------------------------
  $self->copyInpFiles();

  # Export needed environment variables
  # -----------------------------------
  $ENV{U} = $$self{U};

  # Change into WORK of temp area
  # -----------------------------
  chdir($$self{U}."/WORK");

  return $self;
}

# Copy INP and IN1 files from OPT-directory into user area
##############################################################################
sub RUNBPE::copyInpFiles {
  my $self  = shift;

  my $optDir = $$self{OPT_DIR};

  my $pth_opt;
  if (defined $$self{S_PTH_OPT}) {
    $pth_opt = $$self{S_PTH_OPT};
  }
  else {
    $pth_opt = "$$self{U_OLD}/OPT";
  }
  opendir(inDir, "$pth_opt/$optDir");
  while ( defined(my $fileName=readdir(inDir)) ) {
    if ($fileName =~ /.*\.INP/ || $fileName =~ /.*\.IN1/) {
      File::Copy::copy ("$pth_opt/$optDir/$fileName",
                        "$$self{U}/PAN/$fileName");
    }
  }
  closedir(inDir);

  # Set Session Table
  # -----------------
  if (defined $$self{SESSION_TABLE}) {
    $self->putKey("$$self{U}/PAN/MENU.INP", "SESSION_TABLE", $$self{SESSION_TABLE});
  }

  # Set Ranges
  # ----------
  if (defined $$self{V_PLUS}) {
    $self->putKey("$$self{U}/PAN/MENU_VAR.INP", "VAR_PLUS", $$self{V_PLUS});
  }
  if (defined $$self{V_MINUS}) {
    $self->putKey("$$self{U}/PAN/MENU_VAR.INP", "VAR_MINUS", $$self{V_MINUS});
  }

  if (defined $$self{V_PTHPGM}) {
    $self->putKey("$$self{U}/PAN/MENU_PGM.INP", "CMD_PATH", $$self{V_PTHPGM});
  }
}

# Write the protocol file header
##############################################################################
sub RUNBPE::writePrtFileHeader {
  my $self     = shift;
  my $cmd_path = $self->getKey ("$$self{U}/PAN/MENU_PGM.INP", "CMD_PATH");
  chomp( $cmd_path );

  open(prtFl, ">>$$self{PRT_FILE}") || die "Cannot open file $$self{PRT_FILE}";

  printf(prtFl "\n\n PROTOCOL FILE FOR BPE SCRIPT");
  printf(prtFl   "\n ----------------------------");

  printf(prtFl "\n Campaign           : %s",$$self{'CAMP_DRV'}
                                                     . $$self{'CAMPAIGN'});
  printf(prtFl "\n Year               : %s",$$self{'YEAR'});
  printf(prtFl "\n Session            : %s",$$self{'SESSION'});
  printf(prtFl "\n PCF name           : %s",$$self{'PCFFIL'});
  printf(prtFl "\n Script name        : %s",$$self{'SCRIPT'});
  printf(prtFl "\n Path to executables: %s",$cmd_path);
  printf(prtFl "\n Option directory   : %s",$$self{'OPT_DIR'});
  printf(prtFl "\n Process ID         : %s",$$self{'PID'});
  printf(prtFl "\n Sub-process ID     : %s",$$self{'SUB_PID'});
  printf(prtFl "\n Server host        : %s",$$self{'BPE_SERVER_HOST'} || '');
  printf(prtFl "\n Remote host        : %s",hostname."  (system pid: $$)");
  printf(prtFl "\n CPU name           : %s",$$self{'CPU'} || '');
  printf(prtFl "\n Path to work area  : %s",$$self{'U'});
  printf(prtFl "\n User name          : %s",$$self{'USER'} || '');
  printf(prtFl "\n PARAM1             : %s",$$self{'PARAM1'}) if ($$self{'PARAM1'});
  printf(prtFl "\n PARAM2             : %s",$$self{'PARAM2'}) if ($$self{'PARAM2'});
  printf(prtFl "\n PARAM3             : %s",$$self{'PARAM3'}) if ($$self{'PARAM3'});
  printf(prtFl "\n PARAM4             : %s",$$self{'PARAM4'}) if ($$self{'PARAM4'});
  printf(prtFl "\n PARAM5             : %s",$$self{'PARAM5'}) if ($$self{'PARAM5'});
  printf(prtFl "\n PARAM6             : %s",$$self{'PARAM6'}) if ($$self{'PARAM6'});
  printf(prtFl "\n PARAM7             : %s",$$self{'PARAM7'}) if ($$self{'PARAM7'});
  printf(prtFl "\n PARAM8             : %s",$$self{'PARAM8'}) if ($$self{'PARAM8'});
  printf(prtFl "\n PARAM9             : %s",$$self{'PARAM9'}) if ($$self{'PARAM9'});
  printf(prtFl "\n");

  printf(prtFl
      "\n Date         Time      Run time  Pgm.time  Sta Program   Message");
  printf(prtFl "\n --------------------------------------");
  printf(prtFl    "--------------------------------------\n");

  CORE::close(prtFl);

  PRT_MESS ($$self{PRT_FILE}, 'MSG', 'RUNBPE.pm', 'SCRIPT__STARTED');
}

# Write the protocol file header
##############################################################################
sub RUNBPE::writeLogFileHeader {
  my $self     = shift;
  open(logFile, ">>$$self{LOG_FILE}") || die "Cannot open file $$self{LOG_FILE}";
  printf(logFile "LOGFILE_HEADER\n");
  CORE::close(logFile);
}

# Write the protocol file trailer
##############################################################################
sub RUNBPE::writePrtFileTail {
  my $fileName = shift;

  PRT_MESS ($fileName, 'MSG', 'RUNBPE.pm', 'SCRIPT__ENDED');

  open(prtFl, ">>$fileName") || die "Cannot open file $fileName";
  printf(prtFl " --------------------------------------");
  printf(prtFl  "--------------------------------------\n");
  CORE::close(prtFl);
}

# Run a BPE script with reruns
##############################################################################
sub RUNBPE::reRun {
  my $self = shift;

  my $rerunMax = 0;
     $rerunMax = $$self{V_RERUN} if ($$self{V_RERUN});

  my $irc = -1;

  $$self{RERUN_COUNT} = 0;

  for (my $iRun = 0; $iRun <= $rerunMax && $irc != 0; $iRun++) {
    if ($irc != -1) {
      if ($$self{PRT_FILE}) {
        &PRT_MESS($$self{PRT_FILE},'RER','RUNBPE.pm',"SCRIPT  RERUN: $iRun")
      }
      warn("\n\n ### RUNBPE.pm: start rerun: $iRun\n");
      warn(" #################################\n");

      # refresh .INP-files from OPT-dir
      $self->copyInpFiles();
    }

    if($iRun == $rerunMax && $rerunMax > 0) {
      my($cmd_path) = $self->getKeys("CMD_PATH");
      chomp($cmd_path);
      if(-d "${cmd_path}c") {
        $self->putKey("$$self{U}/PAN/MENU_PGM.INP","CMD_PATH","${cmd_path}c");
        &PRT_MESS($$self{PRT_FILE},'RER','RUNBPE.pm',"CMD PATH: ${cmd_path}c")
        if($$self{PRT_FILE});
      }
    }

    $$self{RERUN_COUNT} += 1;

    eval { $self->run };

    if ($@) {
      warn $@;
      $irc = 1;
    } else {
      $irc = 0
    }
  }
  return $irc;
}

# Default run method for BPE client (may be overridden in script)
##############################################################################
sub RUNBPE::run {
  my $self   = shift;
  die "No script specified in BPEclient::run" unless (exists($$self{SCRIPT}));

  delete $$self{GOTO_PID};

  map { $ENV{$_} = $$self{$_} } keys %$self;

  my $dir;
  if (defined $$self{S_PTH_SCRIPT}) {
    $dir = $$self{S_PTH_SCRIPT};
  }
  else {
    $dir = "$$self{U_OLD}/SCRIPT";
  }
  my $rc = system "$dir/$$self{SCRIPT}";

  die "Error running script $dir/$$self{SCRIPT}" if ($rc);

  if (defined $$self{PRT_FILE}) {
    open (PRT,"< $$self{PRT_FILE}");
    while(<PRT>){
      if ( /SCRIPT  STARTED/ ) { delete $$self{GOTO_PID} }
      if ( /GOTO PID/ )        {
        $$self{GOTO_PID} = substr($_,index($_,'GOTO PID'))
      }
    }
    CORE::close(PRT);
  }
}

# Run one top level Bernese program, including the
##############################################################################
sub RUNBPE::RUN_PGMS {

  my $self   = shift;
  my $pgmnam = shift || $$self{PGMNAM};
  my $irc    = 0;

  # Export needed environment variables
  # -----------------------------------
  $ENV{U}      = $$self{U};
  my ($drv)    = ($self->{CAMP_DRV} =~ /\$\{(\w+)\}/);
  $ENV{$drv} ||= $self->{$drv};

  # Handle the error file - make sure that the error file is written
  # ----------------------------------------------------------------
  $self->putKey("$$self{U}/PAN/$pgmnam.INP", "ERRMRG", "0");

  my $SYSERR = $self->getKey("$$self{U}/PAN/$pgmnam.INP", 'SYSERR');

  if (!defined($SYSERR) || $SYSERR eq "") {
    $self->putKey("$$self{U}/PAN/$pgmnam.INP", "SYSERR", "ERROR");
  }

  # Update protocol file
  # --------------------
  PRT_MESS ($$self{PRT_FILE}, 'MSG', $pgmnam, 'PROGRAM_STARTED');

  # update $U/PAN/pgm.INP to $U/INP/pgm.INP
  # ---------------------------------------
  $self->inpOutMenu("","$$self{U}/PAN/$pgmnam.INP",
                                      "$$self{U}/INP/$pgmnam.INP", $pgmnam);

  $SYSERR = $self->getKey("$$self{U}/INP/$pgmnam.INP", 'SYSERR');

  # Read the default path to the executables
  # ----------------------------------------
  my ($cmd_path,@special_path) = $self->getKeys("CMD_PATH","SPECIAL_PATH");
  chomp( $cmd_path );

  # Check the program-specific path
  # -------------------------------
  if ($#special_path == 0 && $special_path[0] =~ /\n/) {
    @special_path = split(/\n/,$special_path[0]);
  }
  for my $line (@special_path) {
    if ($line =~ /\s*"$pgmnam"\s+"(.+)"\s*/) {
      $cmd_path = $1;
      PRT_MESS ($$self{PRT_FILE}, 'MSG', "RUN_PGMS","PROGRAM_PATH:_$cmd_path");
    }
  }

  # Set up alarm signal handler, for running external processes
  # -----------------------------------------------------------
  $SIG{ALRM} = sub {
    syswrite (STDERR, "received ALARM from $pid, killing proc!\n");
    kill (9, $pid);
    die "Time out for $pgmnam"
  };
  my $defaultTimeOut = 0;  # default time out for FORTRAN program: unlimited


  # Run the external process
  # ------------------------
  if ($main::win32) {
    $cmd_path =~ s/\$\{(\w+)\}/\%$1\%/; # replace ${foo} with %foo%
    $irc = system("\"$cmd_path/$pgmnam\" $$self{U}/INP/$pgmnam.INP");
  } else {
    my $resolvedPath = $self->_expandEnv($cmd_path);
    my $pgmPath = $resolvedPath;
    my $ircp = (-e "$pgmPath/$pgmnam");

    my $pgmext = "";
    if (defined($ENV{GROUP})) {
      if ($ENV{GROUP} eq "aiub") {
        if (defined($$self{'PCFFIL'})) {
          $pgmext = ".".substr($$self{'PCFFIL'},0,4)."$$self{'PID'}";
        } else {
          $pgmext = ".".substr($ENV{'PCFFIL'},0,4)."$ENV{'PID'}";
        }

        $ircp = File::Copy::copy ("$resolvedPath/$pgmnam", "$$self{U}/WORK/$pgmnam${pgmext}");
        if ($ircp) {
          $pgmPath = "$$self{U}/WORK";
          chmod(0770,"$pgmPath/$pgmnam${pgmext}");
        }
      }
    }
    if ($ircp){
      if ($self->debug) {
        $irc = system("echo $$self{U}/INP/$pgmnam.INP | $pgmPath/$pgmnam${pgmext}");
      } else {
        $pid = open(KID_TO_WRITE, "|-"); # do a fork, $pid is a global variable
        if ($pid) {  # parent
          alarm($self->{TIMEOUT} || $defaultTimeOut);
          print KID_TO_WRITE "$$self{U}/INP/$pgmnam.INP\n";
          unless (CORE::close(KID_TO_WRITE)) { $irc = $?; }
          alarm(0);
        } else {     # child
          exec("$pgmPath/$pgmnam${pgmext}") || die "can't exec $pgmnam: $!";
        }
      }
    if (defined($ENV{GROUP})) {
      if ($ENV{GROUP} eq "aiub") {
        unlink("$$self{U}/WORK/$pgmnam${pgmext}");
      }
    }
      unlink(glob("$$self{U}/WORK/$pgmnam.SC[R12]"));

    } else {
      print "Program not found: $resolvedPath/$pgmnam\n";
      $irc = 1;
    }
  }

  $$self{SYSOUT}   = $self->getKey ("$$self{U}/INP/$pgmnam.INP", 'SYSOUT');
  chomp ($$self{SYSOUT});
  ($$self{JOBNUM}) = ($$self{SYSOUT} =~ /.*\.L(\d\d)\s*$/);

  my $syserrContents = '';
  if ($SYSERR) {
    $SYSERR =~ s/\$\{(\w+)\}/$$self{$1}/g;
    chomp($SYSERR);
    if (defined ($SYSERR) && -f $SYSERR) {
      if (exists($$self{PRT_FILE}) && -f $$self{PRT_FILE}) {
        open (PRT, ">>$$self{PRT_FILE}");
        open (SYS, $SYSERR);
        read (SYS, $syserrContents, -s $SYSERR);
        CORE::close SYS;
        print PRT $syserrContents;
        CORE::close PRT;
      }
      File::Path::rmtree($SYSERR);
    }
  }

  # Write the last message
  # ----------------------
  if ($irc) {
    PRT_MESS ($$self{PRT_FILE}, 'ERR', $pgmnam, 'ERROR_IN_MAIN_PROGRAM');
    die "Call to $pgmnam failed:\n$syserrContents";
  } else {
    PRT_MESS ($$self{PRT_FILE}, 'MSG', $pgmnam, 'PROGRAM_ENDED');
  }

  return $syserrContents;
}

# Expand Environment Variables in Path
##############################################################################
sub RUNBPE::_expandEnv {
  my $self = shift;
  my $path = shift;

  while ( $path =~ /(\$\{)(\w+)(\})/ ) {
    my $hlp = eval '$$self{$2}';
    if (! defined $hlp) {
      $hlp = eval '$ENV{$2}';
    }
    $path =~ s/(\$\{)(\w+)(\})/$hlp/;
  }
  return $path;
}

# Get the values of a list of keywords (only one call of the menu is necessary)
###############################################################################
sub RUNBPE::getKeys {

  my $self = shift;

  my ($key,$nLine,$value);

  # Define path and global variables for the use outside of BPE client
  # ------------------------------------------------------------------
  my $uu = (ref($self) && defined ($$self{U})) ? $$self{U} : $ENV{U};
  die "Please define user environment U" unless (defined($uu));

  my $varfile = "$uu/WORK/getkey.INP_$$";
  open(IN, ">$varfile");
  my $nKey=0;
  foreach $key (@_) {
    $nKey++;
    if ($key =~ /\$/) {
      print IN "VAR$nKey 1 undef\n  ## widget = comment\n  # $key\n\n";
    } else {
      print IN "VAR$nKey 1 undef\n  ## widget = initmenu; pointer = $key\n\n";
    }
  }
  CORE::close IN;

  $self->inpOutMenu("$uu/PAN/MENU.INP",$varfile, $varfile, 'getKey');

  my @val=();
  for (my $iKey=1;$iKey<=$nKey;$iKey++) {

    if (exists($$self{$_[$iKey-1]})) {
      $val[$iKey-1] = $$self{$_[$iKey-1]}
    } elsif (exists($ENV{$_[$iKey-1]})) {
      $val[$iKey-1] = $ENV{$_[$iKey-1]};
    } else {
      $val[$iKey-1] = $self->_expandEnv($self->getKey($varfile,"VAR$iKey"));
    }

  }

  return @val;
}

# Get the value of one keyword (use the fortran-program ${XG}/GETKEY !)
##############################################################################
sub RUNBPE::getKey {

  my $self = shift;

  my ($val,$irc);

  my $panFileName = shift || '';
  my $keyName     = shift || '';
  my $option      = shift || '';

  if ( $panFileName eq '' || $keyName eq '' ||
      ($option ne '' && $option ne "FILNAM" && $option ne "FILEXT")) {
    die "Usage: $self->getKey (panFileName, keyName, [FILNAM|FILEXT])";
  }

  # Define path and global variables for the use outside of BPE client
  # ------------------------------------------------------------------
  my $xg = (ref($self) && defined ($$self{XG})) ? $$self{XG} : $ENV{XG};
  die "Please define executable directory XG" unless (defined($xg));

  unless(defined($main::win32)) {
    $main::win32 = (uc($ENV{'OS_NAME'}) =~ /^WIN/);
  }

  if ($main::win32) {
    $val = `\"$xg/GETKEY\" $panFileName $keyName $option`;
    $irc = $?;
  } else {
    $val = `echo $panFileName $keyName $option | $xg/GETKEY`;
    $irc = $?;
  }

  die "Error in call to GETKEY: $irc\n $val" if ($irc);

  chomp($val);
  if (ref ($val) =~ /ARRAY/) {
    return @$val;
  } else {
    return $val;
  }
}

# Call the 'menu' program to update an .INP file or to expand a variable
##############################################################################
sub RUNBPE::inpOutMenu {

  my $self    = shift;
  my $optFile = shift || "$$self{U}/PAN/MENU.INP";
  my $infile  = shift;
  my $outfile = shift;
  my $pgmnam  = shift || '';

  my $debug = 0;

  # Generate Menu Input File
  # ------------------------
  my($menuFile) = "$$self{U}/WORK/MENU_$$.TMP";

  open(outFile, ">$menuFile") || die "Cannot open file $menuFile";

  print(outFile "MODJULDATE       1 $$self{MJD}               \n\n");
  print(outFile "SESSION_CHAR     1 $$self{SESSION}           \n\n");
  print(outFile "SERVER_VARIABLES 1 $$self{SERVER_VARIABLES}  \n\n")
                                        if (exists($$self{SERVER_VARIABLES}));
  print(outFile "INP_FILE_NAME    1 $infile  \n\n");
  print(outFile "OUT_FILE_NAME    1 $outfile \n\n");
  print(outFile "ACTIVE_CAMPAIGN  1 $$self{CAMP_DRV}$$self{CAMPAIGN} \n\n");
  print(outFile "JOB_ID           0                           \n\n");

  print("RUNBPE::inpOutMenu: vor CORE::close outfile\n ") if $debug;
  CORE::close(outFile);

  print( "RUNBPE::inpOutMenu: vor startmenu\n ") if $debug;
  die() if ($self->startMenu($optFile,$menuFile,$pgmnam));
  print( "RUNBPE::inpOutMenu: nach startmenu\n ") if $debug;

  return;
}

# Start the menu program in non-interactive mode
##############################################################################
sub RUNBPE::startMenu {

  my $self       = shift;
  my $menuOption = shift;
  my $menuTodo   = shift;
  my $pgmnam     = shift || '';

  my $debug = 0;
  my $dTime = time();

  print( "RUNBPE::startMenu: beginn startmenu ("._sec2hms(time-$dTime).")\n ") if $debug;

  # This method should also be usable outside the client environment
  # ----------------------------------------------------------------
  my $xq = (ref($self) && defined ($$self{XQ})) ? $$self{XQ} : $ENV{XQ};
  die "Please define executable directory XQ" unless (defined($xq));

  print( "RUNBPE::startMenu: nach xq ("._sec2hms(time-$dTime).")\n ") if $debug;

  my $uu = (ref($self) && defined ($$self{U})) ? $$self{U} : $ENV{U};
  die "Please define user environment U" unless (defined($uu));

  print( "RUNBPE::startMenu: nach uu ("._sec2hms(time-$dTime).")\n ") if $debug;

  unless(defined($main::win32)) {
    $main::win32 = (uc($ENV{'OS_NAME'}) =~ /^WIN/);
  }



  # Run the menu
  # ------------
  my $irc;
  if ($main::win32) {

    $irc = system("$xq/menu", "$menuOption", "$menuTodo");
    if (@_) { print @_ }

  } else {

  print( "RUNBPE::startMenu: run the menu ("._sec2hms(time-$dTime).")\n ") if $debug;
    # This copy is necessary during development of the 'menu' program
    # (only RUNBPE_new.pm should use $XQ/menu_new...)
    # ---------------------------------------------------------------
    $irc = system("$xq/menu.sh", "$menuOption", "$menuTodo");
  print( "RUNBPE::startMenu: mach system menu ("._sec2hms(time-$dTime).")\n ") if $debug;
    if (@_) { print @_ }
  print( "RUNBPE::startMenu: nach if print ("._sec2hms(time-$dTime).")\n ") if $debug;

  }


  print( "RUNBPE::startMenu: vor if ("._sec2hms(time-$dTime).")\n ") if $debug;
  if ($irc) {
    if (ref($self)) {
      PRT_MESS ($$self{PRT_FILE}, 'ERR', $pgmnam, 'ERROR_IN_MENU')
                                            if (exists($$self{PRT_FILE}))
    }
    warn "RUNBPE::startMenu: call to menu program failed";
  }

  print( "RUNBPE::startMenu: ende startmenu ("._sec2hms(time-$dTime).")\n ") if $debug;

  return ($irc);
}

# Write a keyword to an Input File (interface to Fortran program
##############################################################################
sub RUNBPE::putKey {

  my $self = shift;

  die "Usage: $self->putKey (panFileName, keyName, value)\n" .
      "or     $self->putKey (panFileName, keyName, value, action, selFile)"
      unless (@_ >= 3);

  my $panFileName = shift;
  my $keyName     = shift;
  my $value       = shift;
  my $action      = shift || '';
  my $selFile     = shift || '';

  # Define path and global variables for the use outside of BPE client
  # ------------------------------------------------------------------
  my $xg = (ref($self) && defined ($$self{XG})) ? $$self{XG} : $ENV{XG};
  die "Please define executable directory XG" unless (defined($xg));

  unless(defined($main::win32)) {
    $main::win32 = (uc($ENV{'OS_NAME'}) =~ /^WIN/);
  }

  # Replace " by \" and enclose string in double quotes
  # ---------------------------------------------------
  $value =~ s/\"/\\\"/g;
#  $value = '"'.$value.'"'; # system dependent, see below

  unless ($selFile) { # three argument call
    if ($main::win32) {
      system "\"$xg\\PUTKEYW\" $panFileName $keyName \\\"$value\\\"";
    } else {
      open (SYS, "| $xg/PUTKEYW");
      print SYS "$panFileName $keyName \"$value\"\n";
      CORE::close SYS;
    }
  } else {            # four argument call
    if ($main::win32) {
      system "\"$xg\\PUTKEYW\" $panFileName $keyName $action $selFile";
    } else {
      open (SYS, "| $xg/PUTKEYW");
      print SYS "$panFileName $keyName $action $selFile\n";
      CORE::close SYS;
    }
  }

  die "Error in call to PUTKEYW: $?" if ($?);
}

# Set up object variables required by both master and slave parallel scripts
##############################################################################
sub RUNBPE::setParallelParams {

  my $self = shift;

  # Local Variables
  # ---------------
  my (@listOfParams, $param, $ipar, $parName, $line);

  # Several Variables Required by old Scripts
  # -----------------------------------------
  $$self{'TMPFIL1'} = "tmpfil1";
  $$self{'TMPFIL2'} = "tmpfil2";

  # Variables required by a Master Script
  # -------------------------------------
  if ( defined($$self{SLAVE}) ) {
    $$self{'tmp1'}         = "ctrfil";
    $$self{'tmp2'}         = "ctrfil";
    $$self{'PARAM1'}       = "ctrfil";
    $$self{'CONTROL_FILE'} = "$$self{'T'}/AUTO_TMP/ctrfil";
  }

  # Variables required by a Slave Script
  # ------------------------------------
  $line = $$self{CONTROL_FILE_LINE};
  if (defined($line)) {
    @listOfParams = split(' ', $line);

    $ipar = 0;
    foreach $param (@listOfParams) {
      $ipar = $ipar + 1;
      $parName = sprintf("PARAM%d", $ipar);
      $param =~ s/\$/\$\$self/g; # Change ${C} to $$self{C}
      $param =~ s/\\/\\\\/g;     # Change \ to \\
      $$self{"$parName"} = eval "qq{$param}";
    }
  }

  return $self;
}

# Accessor function to get the command from a BPEclient object.
##############################################################################
sub RUNBPE::command {
  my $self   = shift;
  return $self->{COMMAND} || '';
}

# Debug mode, true if the DEBUG variable is included in the object.
##############################################################################
sub RUNBPE::debug {
  my $self   = shift;
  return $self->{DEBUG} || 0;
}

# Noclean mode, true if the NOCLEAN variable is included in the object.
##############################################################################
sub RUNBPE::noclean {
  my $self   = shift;
  return $self->{NOCLEAN} || 0;
}

# Name of temporary directory
##############################################################################
sub RUNBPE::tmpDir {
  my $self   = shift;
  return $self->{U} || '';
}

# Append a message to the log file
##############################################################################
sub RUNBPE::PRT_MESS {

# List of Parameters
# ------------------
  my($logFileName, $msgType, $pgmName, $message) = @_;

  if (!defined($message)) {
    die "Usage: PRT_MESS (logFileName, msgType, pgmName, message)\n";
  }

  return unless ($logFileName); # do nothing if no log file specified.

  # Get the Local Time
  # ------------------
  my($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();

  my(@monStr)= ('JAN','FEB','MAR','APR','MAY','JUN',
                'JUL','AUG','SEP','OCT','NOV','DEC');

  my($timeStr) = sprintf("%2.2d-%3s-%2.2d  %2.2d:%2.2d:%2.2d",
                         $mday, $monStr[$mon], $year + 1900,
                         $hour, $min, $sec);

  $message =~ s/_/ /g;

  # Open the log file and read the last line
  # ----------------------------------------
  open(LOGFILE, "<$logFileName") || die "Cannot open file $logFileName";

  my($line);
  my($lastTime) = 0;
  my($sumTime)  = 0;

  while ( defined($line=<LOGFILE>) ) {
    my($hlpTime1, $hlpTime2) = _readTimeString($line);
    if ($hlpTime1 != 0) {
      $lastTime = $hlpTime1;
      $sumTime  = $hlpTime2;
    }
    # Reset time when a "rerun" of the script took place
    if ($line =~ /------------------/) {
      $lastTime = 0;
      $sumTime  = 0;
    }
  }
  CORE::close(LOGFILE);

  # Total time, time spent by the program
  # -------------------------------------
  my $now        = time();
  my $totTimeStr = '00:00:00';
  my $pgmTimeStr = '        ';

  if ( $lastTime != 0 &&  $now - $lastTime >= 0) {
    $totTimeStr = _sec2hms($now - $lastTime + $sumTime);
    if ( $message =~ /(.*)(PROGRAM ENDED)(.*)/         ||
         $message =~ /(.*)(ERROR IN MAIN PROGRAM)(.*)/ ) {
      $pgmTimeStr = _sec2hms($now - $lastTime);
    }
  }

  # Open the log file in append mode
  # --------------------------------
  open(logFile, ">>$logFileName") || die "Cannot open file $logFileName";

  printf(logFile " %s  %s  %s  %s %-9s %s\n", $timeStr, $totTimeStr,
         $pgmTimeStr, $msgType, $pgmName, $message);

  CORE::close(logFile);
}

# Convert number of seconds into HH:MM:SS string
##############################################################################
sub RUNBPE::_sec2hms {

  # List of Parameters
  # ------------------
  my $dt = shift;

  if ($dt <= 0) {
    return "00:00:00";
  }

  # Local Variables
  # ---------------
  my $hour = int($dt / 3600);
  my $min  = int(($dt - ($hour * 3600)) / 60);
  my $sec  = int($dt - $hour * 3600 - $min * 60);

  return sprintf("%2.2d:%2.2d:%2.2d", $hour, $min, $sec);
}

# Read the time string, return number of seconds
##############################################################################
sub RUNBPE::_readTimeString {

  # List of Parameters
  # ------------------
  my $line = shift;

  # Local Variables
  # ---------------
  my($time)    = 0;
  my($sumTime) = 0;
  my($timeStringPattern) = '(\d\d)-(\w\w\w)-(\d\d\d\d)  ' .
                           '(\d\d):(\d\d):(\d\d)  (\d\d):(\d\d):(\d\d)';

  if ($line =~ /^ $timeStringPattern .*/) {
    my($mday)   = $1;
    my($monStr) = $2;
    my($year)   = $3;
    my($hour)   = $4;
    my($min)    = $5;
    my($sec)    = $6;
    my($Dhour)  = $7;
    my($Dmin)   = $8;
    my($Dsec)   = $9;

    my(%monthNumbers) = ('JAN',  0,
                         'FEB',  1,
                         'MAR',  2,
                         'APR',  3,
                         'MAY',  4,
                         'JUN',  5,
                         'JUL',  6,
                         'AUG',  7,
                         'SEP',  8,
                         'OCT',  9,
                         'NOV', 10,
                         'DEC', 11);

    my $mon = $monthNumbers{ $monStr };

    $time    = timelocal($sec, $min, $hour, $mday, $mon, $year - 1900);
    $sumTime = $Dhour * 3600 + $Dmin * 60 + $Dsec;
  }

  return ($time, $sumTime);
}

# Read the string "KEY1=VALUE1 && KEY2=VALUE2 && etc..." into an object
##############################################################################
sub RUNBPE::_loadMsg {

  my $self = shift;
  my $msg  = shift;

  # For windows all necessary environment variables are set globally
  # ----------------------------------------------------------------
  if ($main::win32) {
    my $envVars = $ENV{BERNESE_VARIABLES};
    die "BERNESE_VARIABLES must be defined in the environment"
                                                          unless ($envVars);
    $envVars =~ s/\"//g;      # remove quotes
    for my $var (split (' ', $envVars)) {
      $$self{$var} = $ENV{$var} ||
                die "Variable $var (from BERNESE_VARIABLES) not defined";
    }

  # Unix: source CLIENT_ENV file, fully evaluating all internal variables
  # ---------------------------------------------------------------------
  } else {
    my ($clientEnv) = ($msg =~ /CLIENT_ENV=([\\\:\.\$\{\}\w\/]+)/);
    if (defined($clientEnv)) {
      while ($clientEnv =~ /\$\{(\w+)\}/ ) {
        my $key = $1;
        if (exists ($ENV{$key})) {
          $clientEnv =~ s/\${$key}/$ENV{$key}/;
        } else {
          warn "Cannot find substitution variable $key";
          last;
        }
      }

      open (IN, $clientEnv) || die "Cannot open file $clientEnv";
      while (my $line=<IN>) {

        # Ignore comment lines
        my $ii = index($line,"#");
        if ($ii != -1) {
          my $i1 = index($line,"setenv");
          my $i2 = index($line,"export");

          next if ( $i1 != -1 && $ii < $i1);
          next if ( $i2 != -1 && $ii < $i2);
        }

        if ($line =~ /\s*setenv\s*(\S*)\s*"(.*)"/ ||  # csh syntax
            $line =~ /\s*export\s*(\S*)="(.*)"/) {
          my $var = $1;
          my $val = $2;

          if ($var eq "PATH"){ next }

          while ( $val =~ /\$\{(\w+)\}/ ) {
            my $key = $1;
            if ( exists $$self{$key} ) {
              $val =~ s/\${$key}/$$self{$key}/;
            } elsif (exists ($ENV{$key})) {
              $val =~ s/\${$key}/$ENV{$key}/;
            } else {
              warn "Cannot find substitution variable $key";
              last;
            }
          }
          $$self{$var} = $val;
          $ENV{$var} = $val if exists($ENV{$var});
        }
      }
      CORE::close IN;
    }
  }

  # Add all values to $self, with similar
  # -------------------------------------
  for my $token (split(/\s*&&\s*/,$msg)) {
    if ($token =~ /(.*)=(.*)/ ) {
      my $var = $1;
      my $val = $2;
      if ($var eq "CAMP_DRV") {
        $$self{$var} = $val;
      } else {
        while ( $val =~ /\$\{(\w+)\}/ ) {
          my $key = $1;
          if ( exists $$self{$key} ) {
            $val =~ s/\${$key}/$$self{$key}/;
          } elsif (exists ($ENV{$key})) {
            $val =~ s/\${$key}/$ENV{$key}/;
          } else {
            warn "Cannot find substitution variable $key";
            last;
          }
        }
        $$self{$var} = $val;
      }
    }
  }

  return $self;
}

# Print a GOTO statement into the protocol file.
##############################################################################
sub RUNBPE::PRT_GOTO {
  my $self = shift;
  my $local_pid  = shift;
  $$self{GOTO_PID} = "GOTO PID $local_pid";
  PRT_MESS ($$self{'PRT_FILE'}, 'MSG', $$self{'SCRIPT'},"GOTO_PID_$local_pid");
}

# Run a BPE (perl) Script (used on Unix only from shell scripts)
##############################################################################
sub RUNBPE::RBPE {

  # Parameters
  # ----------
  my($scriptName, $inFile, $outFile, $append) = @_;

  if (!defined($scriptName)) {
    die "Usage: RBPE scriptName [ inFile [ outFile [ APPEND ] ] ]\n";
  }

  if (!defined $inFile) {
    system("$ENV{'BPE'}/$scriptName.pl");
  }
  elsif (!defined($outFile)) {
    system("$ENV{'BPE'}/$scriptName.pl < $inFile");
  }
  else {
    if ($inFile eq "NULL") {
      if (defined($append) && $append eq "APPEND") {
        system("$ENV{'BPE'}/$scriptName.pl >> $outFile");
      }
      else {
        system("$ENV{'BPE'}/$scriptName.pl >  $outFile");
      }
    }
    else {
      if (defined($append) && $append eq "APPEND") {
        system("$ENV{'BPE'}/$scriptName.pl < $inFile >> $outFile");
      }
      else {
        system("$ENV{'BPE'}/$scriptName.pl < $inFile >  $outFile");
      }
    }
  }
}


# Compute time variables
##############################################################################
sub RUNBPE::initTimeVar {
  my $self = shift;

  my @mKey = ( '$YMD_STR-(90)','$WD-(90)','$S-(90)','$JD-(90)',
               '$YMD_STR-(45)','$WD-(45)','$S-(45)','$JD-(45)',
               '$YMD_STR-(30)','$WD-(30)','$S-(30)','$JD-(30)',
               '$YMD_STR-(15)','$WD-(15)','$S-(15)','$JD-(15)',
               '$YMD_STR-8'   ,'$WD-8'   ,'$S-8'   ,'$JD-8'   ,
               '$YMD_STR-3'   ,'$WD-3'   ,'$S-3'   ,'$JD-3'   ,
               '$YMD_STR-2'   ,'$WD-2'   ,'$S-2'   ,'$JD-2'   ,
               '$YMD_STR-1'   ,'$WD-1'   ,'$S-1'   ,'$JD-1'   ,
               '$YMD_STR+1'   ,'$WD+1'   ,'$S+1'   ,'$JD+1'   ,
               '$YMD_STR+2'   ,'$WD+2'   ,'$S+2'   ,'$JD+2'   ,
               '$YMD_STR+3'   ,'$WD+3'   ,'$S+3'   ,'$JD+3'   );

  my @mVal = $self->getKeys(@mKey);

  my %mTime = ();
  for (my $ii=0; $ii <= $#mKey; $ii++) {
    $mKey[$ii] =~ s/-/M/;
    $mKey[$ii] =~ s/\+/P/;
    $mKey[$ii] =~ s/[$()]//g;
    $mTime{$mKey[$ii]} = $mVal[$ii];
  }

  foreach my $key ('M90','M45','M30','M15','M8','M3','M2','M1','P1','P2','P3') {

    # Extract date: $YMD_STR -> yyyy mm dd / yy
    ($$self{"YR_4$key"},$$self{"MONT$key"},$$self{"DAYM$key"}) =
                                        split(" ",$mTime{"YMD_STR$key"});
    $$self{"YEAR$key"} = substr($$self{"YR_4$key"},2,2);  # 2-digit year

    # GPS-Week: $WD -> wwww d
    $$self{"GPSW$key"} = substr($mTime{"WD$key"},0,4);
    $$self{"DAYW$key"} = substr($mTime{"WD$key"},4,1);

    # GPS-Week: $S -> ssss / ddd s
    $$self{"SESS$key"} = $mTime{"S$key"};
    $$self{"DAYY$key"} = substr($mTime{"S$key"},0,3);
    $$self{"SSID$key"} = substr($mTime{"S$key"},3,1);

    # MJD: $JD -> mmmmm
    $$self{"MJD$key"}  = $mTime{"JD$key"};

  }

}

# Script Time
##############################################################################
sub RUNBPE::timeScript {
  my $self = shift;
  return sprintf("%.3f", $$self{FINISHED_TIME} - $$self{START_TIME});
}

# Fortran (Program) Time
##############################################################################
sub RUNBPE::timePgm {
  my $self = shift;

  # Read Fortran User Time(s)
  # -------------------------
  my $fortran_time = 0.0;
  open(inFile,  "<$$self{LOG_FILE}") || die "Cannot open file $$self{LOG_FILE}";
  while ( defined(my $line=<inFile>) ) {
    if    ($line =~ /LOGFILE_HEADER/) {
      $fortran_time = 0.0;
    }
    elsif ($line =~ /CPU\/Real time for pgm/) {
      my @p   = split(' ', $line);
      my @hms = split(':', $p[8]);
      $fortran_time += $hms[0] * 3600.0 + $hms[1] * 60.0 + $hms[2];
    }
  }
  CORE::close(inFile);

  return sprintf("%.3f", $fortran_time);
}

1;
