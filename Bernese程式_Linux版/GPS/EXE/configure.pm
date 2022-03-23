#!/usr/bin/perl -w
# ==============================================================================
#
# Name:       configure.pm
#
# Purpose:    Configuration utility for the Bernese GNSS Software.
#
# Author:     L.Mervart
#
# Created:    13-Aug-2003
#
# Changes:    16-Oct-2003 RD: Path from Bernese either from $ENV{C} or --path
#                             Add $BPE and $X/SCRIPT to change_shebang
#                             Compile menu and fortran in 2 subroutines
#                             Ask for overwrite LOADGPS if not --init
#                             Check entries for LOADGPS before writing the file
#                             Check environment variables before do a task
#                             Install user environment to $U and $T
#                             Add a help screen for --help option
#             06-Dec-2004 HB: Add $JPLEPH variable
#             03-Feb-2005 HB: Add ifc-Compiler, Version 8.1 (LINUX)
#             11-Jan-2012 SL: CBERN COMPLINK and CMENU calls added
#             13-Jan-2012 SL: updated for BSW Version 5.2
#             27-Apr-2012 SL: New test for successful program compilation
#             18-May-2012 SL: MEMSIZE related stuff removed
#             22-May-2012 SL: which-path of Fortran compilers, # of failed $XG
#             23-May-2012 SL: CGROUP variable added to _loadgps
#             11-Jun-2012 SL: FU and XU removed, new defaults for $P, $D, $S
#             26-Jun-2012 SL: Copy of &myWhich from bpe_util.pm
#             14-Sep-2012 SL: Extract DATAPOOL and SAVEDISK archives
#             06-Nov-2012 RD: Set default to the first available Fortran compiler
#             13-Nov-2012 EO: Darwin/GNU added to list of tested compilers
#             20-Nov-2012 EO: Darwin/PG_F90 added to list of tested compilers
#
# Copyright:  Astronomical Institute
#             University of Bern
#             Switzerland
#
# ==============================================================================

return 1 if(caller); # don't execute main if caller

use strict;
use Getopt::Long;
use File::Copy;
use File::Path;
use Sys::Hostname;
use Config;
use User::pwent;

# Main program
################################################################################
{
# ------------------------------------------------------------------------------
#
# Changes:    13-Jan-2012 SL: tests for $$obj{C} and $$obj{QTBERN} changed
#
# ------------------------------------------------------------------------------
  # Command Line Options
  # --------------------
  my $optInit;
  my $optPerl = "";
  my $dirBern;
  my $specQT;
  my $optHelp;
  GetOptions(
    "init"     => \$optInit,    # Init: do all from scratch
    "perl=s"   => \$optPerl,    # Path to Perl, change she-bang lines
    "path=s"   => \$dirBern,    # Path to Bernese GNSS Software
    "qtBern=s" => \$specQT,     # Special Qt lib for Bernese
    "help"     => \$optHelp,    # Print a usage screen
  );

  # Write the usage screen
  # ----------------------
  if(defined($optHelp)) {
    print "\n".
        " configure.pm: Configuration/update tool for the Bernese GNSS Software\n".
        "        The program runs a menu to\n".
        "        1: Update the LOADGPS.setvar file to define the Bernese\n".
        "           environment variables.\n".
        "        2: Check and update the installation.\n".
        "        3: Create a user environment for a new user.\n".
        "        4: Compile the Bernese menu.\n".
        "        5: Compile the Bernese programs.\n".
        "        6: Install the example campaign.\n".
        "\n".
        " Usage: confgure.pm [ options ]\n".
        "        Options may be: \n".
        "        --init: Build Bernese environment from scratch, otherwise\n".
        "                it is expected that the Bernese environment has\n".
        "                already been loaded\n".
        "        --perl=/usr/bin/perl: Replace all she-bang lines\n".
        "                to your location of Perl. This is necessary once\n".
        "                after installation of the source code and is usually\n".
        "                done by the 'setup.sh' script\n".
        "        --path=\$HOME/BERN52: The location of the Bernese GNSS Software\n".
        "                is either taken from this parameter or from the\n".
        "                environment variable \$C.\n".
        "        --qtBern=\$QTDIR: If there is a special installed Qt library\n".
        "                to make run the Bernese menu here can be specified a path\n".
        "                to this library which may be different from the normal\n".
        "                location of the Qt library for other applications in\n".
        "                your system\n".
        "        --help: This help screen.\n".
        "\n";
  }
  else {

    # Create the configure object
    # ---------------------------
    my $obj = configure->new($optInit);

    # Get the path to the Bernese GNSS Software
    # -----------------------------------------
    if(defined($ENV{C})) {  # from environment variable
      $$obj{C} = $ENV{C};
    }
    if(defined($dirBern)) { # from command line parameter
      $$obj{C} = $dirBern;
    }
    unless($$obj{C}) {
      die "\n Please define the path to the Bernese GNSS Software either\n".
            "  - running 'LOADGPS.setvar' before calling configure.pm or\n".
            "  - adding the parameter '--path=' when calling configure.pm\n\n";
    }

    # Check for a special Qt path
    # ---------------------------
    if(defined($ENV{QTDIR})) { # from environment variable $QTDIR
      $$obj{QTBERN} = $ENV{QTDIR};
    }
    if(defined($ENV{QTBERN})) { # from environment variable $QTBERN
      $$obj{QTBERN} = $ENV{QTBERN};
    }
    if(defined($specQT)) {     # from command line parameter
      $$obj{QTBERN} = $specQT;
    }
    unless($$obj{QTBERN}) {
      die "\n *** configure.pm: The Qt library seems not to be installed".
          "\n                   correctly since \$QTDIR is not defined.".
          "\n                   You may add the option '--qtBern='\n\n";
    }

    if(defined($optInit)) {
      $$obj{init} = 1;
    }
    else {
      $$obj{init} = 0;
    }

    # Change the she-bang lines
    # -------------------------
    if($optPerl ne "") {
      $obj->change_shebang($optPerl);
    }

    # Check for programs
    # ------------------
    my @pgmList = $$obj{win} ? () : ("echo","which","gzip","tar");
    foreach(@pgmList) {
      my($exe) = $obj->myWhich($_);
      die " *** Executable $_ is missing!\n" unless(-x $exe);
    }

    # Main loop
    # ---------
    while(1) {
      $obj->displayMenu();
      my $userInput = <STDIN>;
      chomp($userInput);
      if(uc($userInput) eq "X") {
        last;
      }
      eval { $obj->action($userInput) };
      if ($@) {
        print "\n$@\n";
        print "Press Enter to continue...\n";
        my $ans = <STDIN>;
      }
    }
  } # end of "non-help" part
}

# ==============================================================================
package configure;
# ==============================================================================

## Constructor
###############################################################################
sub configure::new {

  my $classname = shift;
  my $self = {};

  bless $self, $classname;

  $self->_init(@_);

  return $self;

}

## Initialize
###############################################################################
sub configure::_init() {
# ------------------------------------------------------------------------------
#
# Changes:    13-Jan-2012 SL: defintion for $$self{win} added
#                             &_updatebsw added from bsw50updater.pm
#             16-Jan-2012 SL: G95,GNU,IFC,CYGWIN added, no unsupported compilers
#             26-Jan-2012 SL: G95 added to all OS, allow OTHER everywhere
#
# ------------------------------------------------------------------------------

  use File::Basename qw(dirname);

  my $self     = shift;
  $$self{init} = shift;
  $$self{first}= 1;

# Windows platform?
# -----------------
##  $$self{win} = (defined $ENV{'OS_NAME'} && uc($ENV{'OS_NAME'}) =~ /^WIN/);
  $$self{win} = ($^O eq "MSWin32");

 # Define the menu items
  # ---------------------
  my %dispatcher = $$self{win} ?
   (
    "1" => \&_onlyUNIX,
    "2" => \&_updatebsw,
    "3" => \&_onlyUNIX,
    "4" => \&_onlyUNIX,
    "5" => \&_complink,
    "6" => \&_onlyUNIX,
    "7" => \&_deleteEnvVars,
   ) : (
    "1" => \&_loadgps,
    "2" => \&_updatebsw,
    "3" => \&_adduser,
    "4" => \&_compmenu,
    "5" => \&_complink,
    "6" => \&_democamp,
    "7" => \&_onlyWIN,
  );
  unless($$self{win}) {
    if($$self{init}) {
      %dispatcher = (
        "0" => \&_complete,
        %dispatcher
      );
    }
  }
  $$self{dispatcher} = { %dispatcher };

  $$self{VERSION} = 52;

# Available Fortran compilers
# ---------------------------
  my %compiler = (
    "LINUX"   => "G95 GNU IFC PG_F90 SUNF90 OTHER",
    "DARWIN"  => "GNU PG_F90 OTHER",
    "CYGWIN"  => "G95 GNU OTHER",
    "MSWIN32" => "AIUB G95 LF95 PG_F90 OTHER",
  );

  $$self{compiler} = { %compiler };

  my %descr_compiler = (
    "LINUX_G95"      => "g95 tested at AIUB",
    "LINUX_GNU"      => "gfortran tested at AIUB",
    "LINUX_IFC"      => "ifort tested at AIUB (version 10 and 12)",
    "LINUX_PG_F90"   => "pgf90 tested at AIUB",
    "LINUX_SUNF90"   => "f90 tested at AIUB",
    "LINUX_OTHER"    => "Use \"OTHER\" compiler from ".&dirname($0)."/Makefile.template",

    "DARWIN_GNU"     => "gfortran tested at AIUB",
    "DARWIN_PG_F90"  => "pgf90 tested at AIUB",
    "DARWIN_OTHER"   => "Use \"OTHER\" compiler from ".&dirname($0)."/Makefile.template",

    "CYGWIN_G95"     => "g95 tested at AIUB",
    "CYGWIN_GNU"     => "gfortran tested at AIUB",
    "CYGWIN_OTHER"   => "Use \"OTHER\" compiler from ".&dirname($0)."/Makefile.template",

    "MSWIN32_AIUB"   => "Official version pre-compiled at AIUB",
    "MSWIN32_G95"    => "g95 tested at AIUB",
    "MSWIN32_LF95"   => "lf95 tested at AIUB",
    "MSWIN32_PG_F90" => "pgf90 tested at AIUB",
    "MSWIN32_OTHER"  => "Use \"OTHER\" compiler from ".&dirname($0)."/Makefile.template",
  );

  $$self{descr_compiler} = { %descr_compiler };

}

## Change she-bang lines in all Perl scripts
## (it is assumed that the original perl is located in "/usr/bin/perl")
###############################################################################
sub configure::change_shebang() {

  my $self   = shift;
  my $PERL   = shift;

  # Loop all directories containing perl scripts
  # --------------------------------------------
  for my $dir("GPS/EXE","GPS/SCRIPT","GPS/USERSCPT","BPE") {
    $self->replace("$$self{C}/$dir/*.pl",
                   "^\#!/usr/bin/perl -w", "\#!$PERL -w");
    $self->replace("$$self{C}/$dir/*.pm",
                   "^\#!/usr/bin/perl -w", "\#!$PERL -w");
    $self->replace("$$self{C}/$dir/*[A-Z0-9]",
                   "^\#!/usr/bin/perl -w", "\#!$PERL -w");
  }

}

## Replace a string
###############################################################################
sub configure::replace() {

  my $self = shift;
  my($glob,$oldStr,$newStr) = @_;

  my $file;
  foreach $file(glob($glob)) {
    next unless(-f $file);
    my($perm) = (stat($file))[2];
    File::Copy::copy($file,"$file.tmp");
    open(inFile, "<$file.tmp");
    open(outFile,">$file");

    my $line;
    while(($line=<inFile>)) {
      $line =~ s/$oldStr/$newStr/g;
      print outFile $line;
    }

    close(outFile);
    close(inFile);
    unlink("$file.tmp");
    chmod($perm,$file);
  }
}

## Display the primary menu
###############################################################################
sub configure::displayMenu() {

  my $self   = shift;
  $self->_clearScreen();

  print "\n==========================================";
  print "\nCONFIGURATION OF THE BERNESE GNSS SOFTWARE";
  print "\n==========================================\n";

  if(defined($ENV{VERSION})) {
    if($ENV{VERSION} != $$self{VERSION}) {
      print "\n ### It seems that Bernese GNSS Software Version $ENV{VERSION}".
            "\n     is loaded. The actual version is $$self{VERSION}.\n\n";
    }
  }

  my $num;
  my %disp = %{$$self{dispatcher}};
  foreach $num (sort keys %disp ) {
    my $ref = $disp{$num};
    my $title = &{$ref}($self,"title");
    print " $num ... $title\n";
  }
  print "\n";
  print " x ... Exit\n\n";
  print "Enter option: ";

}

## Dispatcher Routine
###############################################################################
sub configure::action() {
  my $self   = shift;
  my $action = shift;
  my %disp = %{$$self{dispatcher}};
  if(defined($disp{$action})) {
    &{$disp{$action}}($self);
  }
}

## Clear Screen
###############################################################################
sub configure::_clearScreen() {
  my $self = shift;
  if($$self{first}) {
    $$self{first} = 0;
  } else {
    $self->_userInput("Press Enter to continue");
  }
  if($$self{win}) { system("cls");   }
  else {            system("clear"); }
}

## Complete new installation
###############################################################################
sub configure::_complete() {
  my $self  = shift;
  my $title = shift;
  if(defined($title) && $title eq "title") {
    return "Complete installation (Steps 1 to 5)";
  }
  $self->_loadgps();
  $self->_updatebsw();
  $self->_adduser();
  $self->_compmenu();
  $self->_complink();
}

## Placeholder for WINDOWS (keep same menu numbers for UNIX and WINDOWS)
###############################################################################
sub configure::_onlyUNIX() {
  my $self  = shift;
  my $title = shift;
  if(defined($title) && $title eq "title") {
    return "  ---";
  } else {
    print "\nOnly relevant for UNIX/Linux\n";
  }
}

## Placeholder for UNIX (keep same menu numbers for UNIX and WINDOWS)
###############################################################################
sub configure::_onlyWIN() {
  my $self  = shift;
  my $title = shift;
  if(defined($title) && $title eq "title") {
    return "  ---";
  } else {
    print "\nOnly relevant for Windows\n";
  }
}

## Edit LOADGPS File
###############################################################################
sub configure::_loadgps() {
# ------------------------------------------------------------------------------
#
# Changes:    13-Jan-2012 SL: create backup file
#             16-Jan-2012 SL: F_VERS_LIST,JPLEPH added, EXE_${F_VERS}
#                             test if $SHELL is defined (e.g. for CYGWIN)
#             17-Jan-2012 SL: print file creation time
#             30-Jan-2012 SL: D and S added to %bernVars
#
# ------------------------------------------------------------------------------

  use File::Basename qw(dirname);
  use if($^O eq "MSWin32"),"Win32::TieRegistry",qw[:KEY_];

  my $self  = shift;
  my $title = shift;

  if(defined($title) && $title eq "title") {
    return "Update LOADGPS.setvar";
  }

  # Define some variables
  # ---------------------
  my $OS = ($$self{win}) ? "WIN32" : "UNIX";
  my $OS_NAME = uc($::Config{'osname'});
  my %compiler = %{$$self{compiler}};

  my $F_VERS;
  my %bernVars;
  my $ans;
  my($dir,@dirLst);

  # Init the values: set to default
  # -------------------------------
  if($$self{init}) {
    my $home = $$self{win} ? $ENV{HOMEDRIVE} : '${HOME}';
    %bernVars = (
      VERSION         => $$self{VERSION},
      F_VERS          => $self->_defaultFortran($OS_NAME,$F_VERS),
      F_VERS_LIST     => '',
      C               => $$self{C},
      X               => '${C}/GPS',
      LG              => '${C}/LIB/FOR',
      I               => '${C}/INC/FOR',
      FG              => '${C}/PGM/FOR',
      XG              => '${C}/PGM/EXE_${F_VERS}',
      XQ              => '${C}/MENU',
      BPE             => '${C}/BPE',
      BPE_SERVER_HOST => Sys::Hostname::hostname,
      U               => $home.'/GPSUSER'.$$self{VERSION},
      T               => $home.'/GPSTEMP',
      P               => $home.'/GPSDATA/CAMPAIGN'.$$self{VERSION},
      D               => $home.'/GPSDATA/DATAPOOL',
      S               => $home.'/GPSDATA/SAVEDISK',
      QTBERN          => $$self{QTBERN},
      OS              => $OS,
      OS_NAME         => $OS_NAME,
      JPLEPH          => 'DE405',
      CGROUP          => 'USERS',
    );
  }

  # Init the values: set from environment variables
  # -----------------------------------------------
  elsif(
    defined($ENV{VERSION})         &&
    defined($ENV{F_VERS})          &&
    defined($ENV{F_VERS_LIST})     &&
    defined($ENV{BPE_SERVER_HOST}) &&
    defined($ENV{U})               &&
    defined($ENV{T})               &&
    defined($ENV{P})               &&
    defined($ENV{D})               &&
    defined($ENV{S})               &&
    defined($ENV{QTBERN})          &&
    defined($ENV{OS})              &&
    defined($ENV{OS_NAME})         &&
    defined($ENV{JPLEPH})          &&
    defined($ENV{CGROUP})
  ) {
    %bernVars = (
      VERSION         => $ENV{VERSION},
      F_VERS          => $self->_defaultFortran($ENV{OS_NAME},$ENV{F_VERS}),
      F_VERS_LIST     => $ENV{F_VERS_LIST},
      C               => $$self{C},
      X               => '${C}/GPS',
      LG              => '${C}/LIB/FOR',
      I               => '${C}/INC/FOR',
      FG              => '${C}/PGM/FOR',
      XG              => '${C}/PGM/EXE_${F_VERS}',
      XQ              => '${C}/MENU',
      BPE             => '${C}/BPE',
      BPE_SERVER_HOST => $ENV{BPE_SERVER_HOST},
      U               => $ENV{U},
      T               => $ENV{T},
      P               => $ENV{P},
      D               => $ENV{D},
      S               => $ENV{S},
      QTBERN          => $ENV{QTBERN},
      OS              => $ENV{OS},
      OS_NAME         => $ENV{OS_NAME},
      JPLEPH          => $ENV{JPLEPH},
      CGROUP          => $ENV{CGROUP},
    );

    my $key;
    my @Keys = qw(U T);
    foreach $key(@Keys) {
      $bernVars{$key} =~ s/$ENV{HOME}/\$\{HOME\}/;
    }
  }

  # LOADGPS was not loaded before...
  # --------------------------------
  else {
    print "The Bernese environment is not loaded.\n".
          "  - Load Bernese environment before starting 'configure.pm' \n".
          "    to update an existing 'LOADGPS.setvar' file.\n".
          "  - Restart program with 'configure.pm --init' to create \n".
          "    a new 'LOADGPS.setvar' file.\n";
    return;
  }

  # Descriptions for the variables
  # ------------------------------
  my %descVars = (
    VERSION         => "Version of Bernese GNSS Software",
    F_VERS          => "Fortran compiler name",
    F_VERS_LIST     => "List of additional compilers",
    C               => "Path to software",
    X               => "Path to Bernese data files",
    LG              => "Fortran source of subroutines",
    I               => "Fortran source of include and modules",
    FG              => "Fortran source of programs",
    XG              => "Executables of Bernese Fortran programs",
    XQ              => "Executable and source of Bernese menu",
    BPE             => "Scripts for the BPE",
    BPE_SERVER_HOST => "Host of the BPE server",
    U               => "Path to user environment",
    T               => "Path to temp. user environment",
    P               => "Path to campaign area",
    D               => "Path to datapool area",
    S               => "Path to savedisk area",
    QTBERN          => "Path to Qt libraries",
    OS              => "Operating system group",
    OS_NAME         => "Operating system name",
    JPLEPH          => "Number of JPL ephemeris",
    CGROUP          => "Name of group",
  );

  # Display the current settings
  # ----------------------------
  my @Keys = qw(C QTBERN OS OS_NAME F_VERS F_VERS_LIST
                BPE_SERVER_HOST U T P D S);
  my $err = 0;
  my($descr,$value);
  while(1) {
    $$self{first} = 1;
    $self->_clearScreen();
    print "\nCurrent Values:\n--------------\n".
          "    VARIABLE DESCRIPTION           ".
          "VARIABLE NAME      VARIABLE VALUE\n";
    for(my $i = 0;$i <= $#Keys;$i++) {

      # Get the value of the variable
      if(defined($bernVars{$Keys[$i]})) {
        $value = $bernVars{$Keys[$i]};
      } else {
        $value = " ";
      }

      # Get the description of the variable
      if(defined ($descVars{$Keys[$i]})) {
        $descr = $descVars{$Keys[$i]};
      } else {
        $descr = " ";
      }

      # Print the value with/without description
      printf("%2d: %-30s %-15s => %s\n",$i+1,$descr,$Keys[$i],$value);
    }

    # Confirm settings
    # ----------------
    my $ans = $self->_yesNo("\nAccept the values (y/n): ");
    if(uc($ans) eq "Y") {

      # Check that all variables have valid values
      # ------------------------------------------
      $err = 0;
      for(my $i = 0;$i <= $#Keys;$i++) {
        if(defined ($bernVars{$Keys[$i]})) { next }
        if($err) {
          print "Mandatory variable $Keys[$i] is undefined\n";
        } else {
          print "\nMandatory variable $Keys[$i] is undefined\n";
        }
        $err++;
      }
      if($err) { $ans = "N" }    # Empty variables found
    }

    # Check results: Compiler and Operating system
    # --------------------------------------------
    if(uc($ans) eq "Y" &&
         (##$bernVars{OS} ne "UNIX" ||
          !defined $compiler{$bernVars{OS_NAME}} ||
          $compiler{$bernVars{OS_NAME}} !~ /$bernVars{F_VERS}/)) {
      $ans = $self->_yesNo("\n".
           "Your compiler / operating system settings are not\n".
           "supported in '\$X/EXE/Makefile.template'.\nContinue anyhow (y/n):");
    }

    # Check the Bernese direcotries
    # -----------------------------
    if(uc($ans) eq "Y" && !-d $bernVars{C}) {
      $ans = $self->_yesNo("\n".
           "The variable 'C' does not point to an existing path.\n".
           "Continue anyhow (y/n):");
    }

    if(uc($ans) eq "Y" ) {
      @dirLst = qw(X I BPE LG FG XQ);
      foreach (@dirLst) {
        $dir = $bernVars{$_};
        $dir =~ s/^\$\{C\}/$bernVars{C}/;
        if (!-d $dir) {
          print "\nThe Bernese directory structure seems not to be\n".
                "complete in $bernVars{C}. Restart installation\n".
                "with 'setup.sh' using this path for installation.\n";
          return;
        }
      }
    }

    # Leave the questionare or make some more changes...
    # --------------------------------------------------
    if(uc($ans) eq "Y") {
      last
    } else {
      print "\nFor changing a variable, type its number and new value.";
      print "\nNote: If you want to change '1: Path to the software' you have".
            "\n      to start the installation again with \"setup.sh\".\n";
      print "\nExample:\n7 \$(hostname)\n\n";
      my $line = <STDIN>;chomp($line);
      my($i,$newValue) = split(/\s+/, $line); --$i;
      if($i>-1 && defined($Keys[$i])) {
        $bernVars{$Keys[$i]} = $newValue;
      } else {
        print "\n ### Variable number ".scalar($i+1)." is not defined!\n";
      }
    }
  } # End of questionare loop

  # Save the environment variables
  # ------------------------------
  $ENV{C} = $bernVars{C};
  my $key;
  while(($key,$value) = each %bernVars) {
    $ENV{$key} = $self->_expandEnv( $value );

    if($$self{win} && $key ne "OS") {
      $value =~ s/\$\{/%/g;
      $value =~ s/\}/%/g;
      $value =~ s/\//\\/g;
      my $hk_env = Win32::TieRegistry->new(
##        'HKEY_LOCAL_MACHINE/SYSTEM/CurrentControlSet/Control/Session Manager/Environment',
        'HKEY_CURRENT_USER/Environment',
        {Access => KEY_READ() | KEY_WRITE(), Delimiter => '/'}
      );
      my $reg = "HKEY_CURRENT_USER";
      if(defined $hk_env) {
        if($hk_env->GetValue("$key") && $hk_env->GetValue("$key") ne $value) {
          print "".$hk_env->GetValue("$key")." replaced by $value ".
                "for key $key in $reg\n";
        }
        $hk_env->SetValue("$key",$self->_expandEnv("$value"));
        $hk_env->SetValue("USER",$ENV{USERNAME});
        $hk_env->SetValue("BERNESE_VARIABLES","P T X U USER");
      } else {
        print " ### Could not set \$$key in $reg Environment.\n";
      }
    }

  }

  # Add some Bernese paths to the $PATH variable
  # --------------------------------------------
  if($ENV{PATH} !~ /$ENV{X}\/EXE/) { $ENV{PATH} = "$ENV{PATH}:$ENV{X}/EXE" }
  if($ENV{PATH} !~ /$ENV{XG}/)     { $ENV{PATH} = "$ENV{PATH}:$ENV{XG}" }
  if($ENV{PATH} !~ /$ENV{XQ}/)     { $ENV{PATH} = "$ENV{PATH}:$ENV{XQ}" }

  # Save the LOADGPS file
  # ---------------------
  my $loadgpsFile = $self->_expandEnv("$$self{C}/GPS/EXE/LOADGPS.setvar");

  # Overwrite the existing file?
  # ----------------------------
  if(-s $loadgpsFile) {
    my $ans = $self->_yesNo("\nOverwrite existing file $loadgpsFile (y/n): ");
    if(uc($ans) eq "N") {
      print "\nFile $loadgpsFile has  n o t  been updated\n";
      return;
    } else {
      File::Copy::copy($loadgpsFile,"$loadgpsFile.bck");
    }
  }

  # Start writing the LOADGPS-file
  # (different syntax for csh and others)
  # -------------------------------------
  my($csh) = 0;
  $csh = ($ENV{SHELL} =~ /csh/) if(defined($ENV{SHELL}));

  unless (open(outFile,">$loadgpsFile")) {
    print "Cannot write into file $loadgpsFile\n";
    return;
  }

  my $comm = (!$$self{win}) ? "#" : "REM";

  print outFile "$comm "."="x78,"\n$comm",
              "\n$comm Name:       $loadgpsFile\n$comm",
              "\n$comm Created:    ".localtime(time)." by configure.pm\n$comm",
              "\n$comm Changes:\n$comm",
              "\n$comm "."="x78,"\n\n";

  if(!$csh && !$$self{win}) {
    print outFile '# Function to add path to $PATH only once'   ."\n";
    print outFile '# ---------------------------------------'   ."\n";
    print outFile 'addtopath () {'                              ."\n";
    print outFile '  if [ "`echo $PATH | grep $1`" != "$PATH" ]'."\n";
    print outFile '  then'                                      ."\n";
    print outFile '    export PATH="${PATH}:$1"'                ."\n";
    print outFile '  fi'                                        ."\n";
    print outFile '}'                                           ."\n";
  }

  # Write the list of variables
  # ---------------------------
  @Keys = qw(VERSION F_VERS F_VERS_LIST C X LG I FG XG XQ BPE BPE_SERVER_HOST
             U T P D S QTBERN OS OS_NAME JPLEPH CGROUP);

  foreach $key(@Keys) {
    if(defined $descVars{$key}) {
      print outFile "\n$comm $descVars{$key}\n$comm "."-" x length($descVars{$key}) ."\n";
    }
    if($csh) {
      printf(outFile "setenv %-15s \"%s\"\n",$key,$bernVars{$key});
    }
    elsif($$self{win}) {
      $bernVars{$key} =~ s/\$\{/%/g;
      $bernVars{$key} =~ s/\}/%/g;
      $bernVars{$key} =~ s/\//\\/g;
      printf(outFile "set %s=%s\n",$key,$bernVars{$key});
    }
    else {
      printf(outFile "export %s=\"%s\"\n",$key,$bernVars{$key});
    }
  }

  # Add some path to $PATH
  # ----------------------
  print outFile "\n$comm Add Bernese paths to \$PATH\n".
                "$comm --------------------------\n";
  if($csh) {
    print outFile 'if ( "`echo $PATH | grep $XG`" != "$PATH" ) then'   ."\n";
    print outFile '  setenv PATH "${PATH}:$XG"'                        ."\n";
    print outFile 'endif'                                              ."\n";
    print outFile ''                                                   ."\n";
    print outFile 'if ( "`echo $PATH | grep $XQ`" != "$PATH" ) then'   ."\n";
    print outFile '  setenv PATH "${PATH}:$XQ"'                        ."\n";
    print outFile 'endif'                                              ."\n";
    print outFile ''                                                   ."\n";
    print outFile 'if ( "`echo $PATH | grep $X/EXE`" != "$PATH" ) then'."\n";
    print outFile '  setenv PATH "${PATH}:$X/EXE"'                     ."\n";
    print outFile 'endif'                                              ."\n";
  } elsif(!$$self{win}) {
    print outFile "addtopath \"\$X/EXE\"\n";
    print outFile "addtopath \"\$XG\"\n";
    print outFile "addtopath \"\$XQ\"\n";
  }

  close(outFile);

  print "\n","*"x70;
  print "\n* $loadgpsFile";
  print "\n* has been updated.";
  print "\n","*"x70,"\n\n";

  # Create module management file for old IFC(LINUX) compiler
  # ---------------------------------------------------------
  if("$bernVars{OS_NAME}" eq "LINUX" && "$bernVars{F_VERS}" eq "IFC_V6") {
    my $workFile = "$ENV{I}/work.pcl";
    if(!-e $workFile) {
      open(WFILE,"> $workFile");
      print WFILE "work.pc\n$ENV{I}/work.pc\n";
      close WFILE;
    }
  }

  # Create campaign area if it does not exist
  # -----------------------------------------
  if(!-d $ENV{P} &&
    uc($self->_yesNo("\n".
           "Your campaign area \${P} does not exist:\n$ENV{P}\n".
           "Create it now (y/n): ")) eq "Y") {
    File::Path::mkpath("$ENV{P}");
  }

  # Create datapool and savedisk areas if they do not exist
  # -------------------------------------------------------
  if(!-d $ENV{D} &&
    uc($self->_yesNo("\n".
           "Your datapool area \${D} does not exist:\n$ENV{D}\n".
           "Create it now (y/n): ")) eq "Y") {
    File::Path::mkpath("$ENV{D}");
  }

  if(!-d $ENV{S} &&
    uc($self->_yesNo("\n".
           "Your savedisk area \${S} does not exist:\n$ENV{S}\n".
           "Create it now (y/n): ")) eq "Y") {
    File::Path::mkpath("$ENV{S}");
  }

}

## Delete environment variables from User Registry
###############################################################################
sub configure::_deleteEnvVars() {

  use if($^O eq "MSWin32"),"Win32::TieRegistry",qw[:KEY_];

  my $self  = shift;
  my $title = shift;

  if(defined($title) && $title eq "title") {
    return "Remove BSW keys from environment";
  }

  my @Keys = qw(VERSION F_VERS F_VERS_LIST C X LG I FG XG XQ BPE BPE_SERVER_HOST
                U T P D S QTBERN OS_NAME JPLEPH CGROUP USER BERNESE_VARIABLES);

  if($$self{win}) {
    print "\nList of BSW keys in the user environment:\n\n";
    my $hk_env = Win32::TieRegistry->new(
       'HKEY_CURRENT_USER/Environment',
       {Access => KEY_READ() | KEY_WRITE(), Delimiter => '/'}
    );
    foreach my $key(@Keys) {
      if(defined($hk_env)) {
        printf("Key: %-20s -> Value: %s\n",$key,$hk_env->GetValue($key));
      } else {
        printf("Key: %-20s\n",$key);
      }
    }

    my $ans = $self->_yesNo("\nRemove the keys (y/n): ");
    if(uc($ans) eq "Y") {
      foreach my $key(@Keys) {
        if(defined($hk_env)) {
          $hk_env->DELETE($key);
        } else {
          print " ### Could not automatically delete \$$key.\n";
        }
      }
    }
  }

}

# Expand Environment Variables in String
################################################################################
sub configure::_expandEnv {
  my $self   = shift;
  my $string = shift;
  if($$self{win}) {
    while($string =~ /(\%)(\w+)(\%)/) {
      my $hlp = eval '$ENV{$2}';
      $string =~ s/(\%)(\w+)(\%)/$hlp/;
    }
  } else {
    while($string =~ /(\$\{)(\w+)(\})/) {
      my $hlp = eval '$ENV{$2}';
      $string =~ s/(\$\{)(\w+)(\})/$hlp/;
    }
  }
  return $string;
}

## Yes/No Answer
################################################################################
sub configure::_yesNo() {
  my $self = shift;
  my $text = shift;
  while(1) {
    print $text;
    my $userInput = <STDIN>;
    chomp($userInput);
    if(uc($userInput) eq "Y") {
      return "Y";
    }
    elsif(uc($userInput) eq "N") {
      return "N";
    }
  }
}

## User Input
################################################################################
sub configure::_userInput() {
# ------------------------------------------------------------------------------
#
# Changes:    16-Jan-2012 SL: alternative for print call
#
# ------------------------------------------------------------------------------
  my $self    = shift;
  my $text    = shift;
  my $default = shift;
  my @items   = @_;
  my ($item, $userInput);

  my $found = 0;
  until($found) {

# get user input
#    print "\n$text  [$default]: ";
    print defined $default ? "\n$text  [$default]: " : "\n$text  ";
    $userInput = <STDIN>;
    chomp $userInput;

# use default
    $userInput = $default unless $userInput;

# check items
    if(@items) {
      foreach $item(@items) {$found=1 if $userInput eq $item}
    } else {
      $found=1;
    }
  }

  return $userInput;
}

## Fortran compiler
###############################################################################
sub configure::_defaultFortran() {

  my $self    = shift;
  my $OS_NAME = shift;
  my $F_VERS  = shift;
  my $i;

  # available compilers
  my %compiler = %{$$self{compiler}};
  my %descr_compiler = %{$$self{descr_compiler}};

  # default
  my $sel=1;

  # get compilers for OS
  if(defined $compiler{$OS_NAME}) {
    $_ = $compiler{$OS_NAME};
  } else {
    print  "\nNo compiler known for OS $OS_NAME\n";
    return "OTHER";
  }
  my @compilers = split;

  # Write the Compiler test information
  # -----------------------------------
  print "\nInformation about compiler tests for OS $OS_NAME:\n";
  for($i=1; $i<=@compilers; $i++) {
    if(defined $descr_compiler{"${OS_NAME}_$compilers[$i-1]"}) {
      print " $i: ".$descr_compiler{"${OS_NAME}_$compilers[$i-1]"}."\n";
    } else {
      print " $i: \n";
    }
  }

  # Generate and display selection list
  # -----------------------------------
  my @items=();
  print "\nSelect Compiler for OS $OS_NAME:\n";

  for($i=1; $i<=@compilers; $i++) {
    my($tst) = (split(/ /,$descr_compiler{"${OS_NAME}_$compilers[$i-1]"}))[0];
    $tst.= ".exe" if($$self{win});
    if($self->myWhich($tst)) {
      $tst = " -> ".$self->myWhich($tst)."\n";
      $F_VERS = $compilers[$i-1] unless defined $F_VERS;
      $F_VERS = $compilers[$i-1] if $F_VERS eq "";
    } else {
      $tst = "\n";
    }
    print " $i: $compilers[$i-1]$tst";
    push @items,$i;
    $sel = $i if defined $F_VERS && $F_VERS eq $compilers[$i-1];
  }

  # Get user input
  # --------------
  $sel = $self->_userInput("Select",$sel,@items);

  return $compilers[$sel-1];
}

## Add a new user
###############################################################################
sub configure::_adduser() {

  use File::Basename qw(basename dirname);
  use if($^O eq "MSWin32"),"Win32::OLE";

  my $self  = shift;
  my $title = shift;

  if(defined($title) && $title eq "title") {
    my $text = "Add a new user environment";
    $text .= " (only after Step 1)" unless ( defined ($ENV{C}) );
    return $text;
  }

  # Check environment variables
  if(!defined($ENV{U})) {
    print "Variable \${U} is not defined!\n".
          "Load Bernese environment using 'LOADGPS.setvar'\n".
          "before restarting 'configure.pm' program.\n";
    return;
  }

  if(!defined($ENV{T})) {
    print "Variable \${T} is not defined!\n".
          "Load Bernese environment using 'LOADGPS.setvar'\n".
          "before restarting 'configure.pm' program.\n";
    return;
  }

  # Does a user environment already exist?
  if(-d $ENV{U}) {
    print "\nUser environment $ENV{U} already exists.\n";
    if(uc($self->_yesNo("Update files (y/n): ")) eq "N") {
      return;
    }
  }
  else {
    if(uc($self->_yesNo("\nCreate user environment $ENV{U} (y/n): ")) eq "N") {
      return;
    }
  }

  # Create directory structure
  File::Path::mkpath("$ENV{U}");
  File::Path::mkpath("$ENV{U}/OPT");
  File::Path::mkpath("$ENV{U}/OUT");
  File::Path::mkpath("$ENV{U}/PAN");
  File::Path::mkpath("$ENV{U}/PCF");
  File::Path::mkpath("$ENV{U}/SCRIPT");
  File::Path::mkpath("$ENV{U}/WORK");
  File::Path::mkpath("$ENV{T}");

  # Copy files into GPSUSER-environment
  print "\nCopying menu and program input files...\n";
  map { File::Copy::copy($_,"$ENV{U}/PAN/") } glob("$$self{C}/GPS/PAN/*.INP");
  map { File::Copy::copy($_,"$ENV{U}/PAN/USER.CPU") } glob("$$self{C}/GPS/PAN/*.CPU");

  print "Copying BPE user scripts...\n";
  map { File::Copy::copy($_,"$ENV{U}/SCRIPT/") } glob("$$self{C}/GPS/USERSCPT/*");

  print "Copying examples for process control files...\n";
  map { File::Copy::copy($_,"$ENV{U}/PCF/") } glob("$$self{C}/GPS/PCF/*.{PCF,README}");

  print "Copying BPE options for processing examples...\n";
  map { if(-d $_) {
          File::Path::mkpath("$ENV{U}/OPT/".&basename($_)); } } glob("$$self{C}/GPS/OPT/*");

  my $uFile;
  foreach (glob("$$self{C}/GPS/OPT/*/*INP")) {
    File::Copy::copy($_,"$ENV{U}/OPT/".&basename(&dirname($_)));
  }

  print "Copying ICONS ...\n";
  if(!-e "$$self{C}/GPS/DOC/ICONS.tgz") {
    print " ### Archive of ICONS not found!\n";
  }
  else {
    File::Copy::copy("$$self{C}/GPS/DOC/ICONS.tgz","$$self{C}/GPS/DOC/ICONS.tar.Z");

    print "Extracting ICONS ...\n\n";
    chdir ("$$self{C}/GPS/DOC");
    system("gzip -dc ICONS.tar.Z | tar -xf -");
    unlink("ICONS.tar.Z");
  }
  print "\n","*"x70;
  print "\n* User area $ENV{U}".
        "\n* has been added/updated.";
  print "\n","*"x70,"\n\n";

  if($$self{win}) {
    my($WShell,$sc);
    $WShell = Win32::OLE->new('WScript.Shell');
    $sc = $WShell->CreateShortcut("\"$ENV{USERPROFILE}\"/Desktop/Bernese_GNSS_Software.lnk");
    $sc->{'TargetPath'}       = "\"$ENV{XQ}\"/menu.exe";
    $sc->{'IconLocation'}     = "\"$ENV{XQ}\"/menu.ico";
    $sc->{'Arguments'}        = "\"$ENV{U}\"/PAN/MENU.INP";
    $sc->{'WorkingDirectory'} = "\"$ENV{U}\"/WORK";
    $sc->{'Description'}      = "Bernese GNSS Software";
    $sc->Save();
    $WShell = Win32::OLE->new('WScript.Shell');
    $sc = $WShell->CreateShortcut("\"$ENV{USERPROFILE}\"/Desktop/GPSUSER$$self{VERSION}.lnk");
    $sc->{'TargetPath'}       = "\"$ENV{U}\"";
    $sc->{'IconLocation'}     = "\"$ENV{U}\"/GPSUSER$$self{VERSION}.ico";
    $sc->{'Description'}      = "User environment";
    $sc->Save();
    $WShell = Win32::OLE->new('WScript.Shell');
    $sc = $WShell->CreateShortcut("\"$ENV{USERPROFILE}\"/Desktop/CAMPAIGN$$self{VERSION}.lnk");
    $sc->{'TargetPath'}       = "\"$ENV{P}\"";
    $sc->{'IconLocation'}     = "\"$ENV{P}\"/CAMPAIGN$$self{VERSION}.ico";
    $sc->{'Description'}      = "Campaign area";
    $sc->Save();
    $WShell = Win32::OLE->new('WScript.Shell');
    $sc = $WShell->CreateShortcut("\"$ENV{USERPROFILE}\"/Desktop/DATAPOOL.lnk");
    $sc->{'TargetPath'}       = "\"$ENV{D}\"";
    $sc->{'IconLocation'}     = "\"$ENV{D}\"/DATAPOOL.ico";
    $sc->{'Description'}      = "Datapool area";
    $sc->Save();
    $WShell = Win32::OLE->new('WScript.Shell');
    $sc = $WShell->CreateShortcut("\"$ENV{USERPROFILE}\"/Desktop/SAVEDISK.lnk");
    $sc->{'TargetPath'}       = "\"$ENV{S}\"";
    $sc->{'IconLocation'}     = "\"$ENV{S}\"/SAVEDISK.ico";
    $sc->{'Description'}      = "Savedisk area";
    $sc->Save();
  }

}

## Compile the software: Menu
################################################################################
sub configure::_compmenu() {
# ------------------------------------------------------------------------------
#
# Changes:    12-Jan-2012 SL: adapted for Qt4, use 'CMENU -all'
#
# ------------------------------------------------------------------------------
  my $self  = shift;
  my $title = shift;
  if(defined($title) && $title eq "title") {
    my $text = "Compile the menu";
    $text .= " (only after Step 1)" unless ( defined ($ENV{C}) );
    return $text;
  }

  # Make the Menu
  # -------------
  if(!defined($ENV{XQ}) ) {
    print "Variable \${XQ} is not defined!\n".
          "Load Bernese environment using 'LOADGPS.setvar'\n".
          "before restarting 'configure.pm' program.\n";
    return;
  }

  unless(chdir($ENV{XQ})) {
    print "Cannot find directory $ENV{XQ}\n";
    return;
  }

  # Reset "QTLIB" to special Bernese Qt path
  my $oldQTDIR     = "$ENV{QTDIR}"     if(defined $ENV{QTDIR});
  my $oldQMAKESPEC = "$ENV{QMAKESPEC}" if(defined $ENV{QMAKESPEC});
  $ENV{QTDIR}      = $ENV{QTBERN};
##  $ENV{QMAKESPEC} = "";  # take the default

  if($ENV{OS_NAME} eq 'DARWIN') {
    unless (-d "$ENV{QTDIR}/qt_menu.nib") {
      print "
**********************************************************************
WARNING: THE MENU WILL NOT BE COMPILED!!! qt_menu.nib not found!
         As a Mac user, the qt_menu.nib directory is expected to be
         found in the Qt installation directory
         $ENV{QTDIR}
         Please refer to the README_INSTALL.TXT for instructions and
         rerun the configure.pm utility to compile the menu.
**********************************************************************\n\n";
      return;
    }
  }

  my $qmake = ($$self{win}) ? "$ENV{QTDIR}/bin/qmake.exe" :
                              "$ENV{QTDIR}/bin/qmake";
  if(!-x $qmake) {
    print "Program $qmake not found.\nPlease check your Qt installation.";
    return;
  }

  my $menuLog = "$ENV{XQ}/MENUCOMP.log";

  print "\nRunning the compilation of the menu.\n".
        "This can take a while...\n".
        "It depends on the computer performance.\n\n".
        "All compilation output is redirected into file\n".
        "$menuLog\n\n";

  system("echo \"$ENV{X}/EXE/CMENU -all >$menuLog 2>&1\"|sh");

  $ENV{QTDIR}     = $oldQTDIR     if(defined $oldQTDIR);
  $ENV{QMAKESPEC} = $oldQMAKESPEC if(defined $oldQMAKESPEC);

  if(-x "$ENV{XQ}/menu" && -x "$ENV{XQ}/menu_tmp") {
    print "\n","*"x70;
    print "\n* Bernese menu compiled successfully.";
    print "\n","*"x70,"\n\n";
  } else {
    print "#####################################\n";
    print "# Error compiling the Bernese menu! #\n".
          "# Please check the log file...      #\n";
    print "#####################################\n\n";
  }

}

## Compile the software: Fortran programs
################################################################################
sub configure::_complink() {
# ------------------------------------------------------------------------------
#
# Changes:    16-Jan-2012 SL: use CBERN COMPLINK, run makemake.pl
#
# ------------------------------------------------------------------------------
  my $self  = shift;
  my $title = shift;

  if(defined($title) && $title eq "title") {
    my $text = "Compile the programs";
    unless ( $$self{win} ) {
      $text .= " (only after Step 1)" unless ( defined ($ENV{C}) );
    }
    return $text;
  }

  # Compile the Fortran programs
  # ----------------------------
  if(!defined($ENV{X}) ) {
    unless ( $$self{win} ) {
      print "Variable \${X} is not defined!\n".
            "Load Bernese environment using 'LOADGPS.setvar'\n".
            "before restarting 'configure.pm' program.\n";
    } else {
      print "Variable \%X\% is not defined!\n".
            "Activate your Bernese environment by rebooting your system\n".
            "before restarting 'configure.pm' program.\n";
    }
    return;
  }

  my $complinkLog = "$ENV{X}/EXE/COMPLINK.log";

  print "\nRunning the compilation of the Fortran programs.\n".
        "This can take a while...\n".
        "It depends on the computer performance.\n\n".
        "All compilation output is redirected into file\n".
        "$complinkLog\n\n";

  unlink("$$self{C}/CBERN.lock") if(-f "$$self{C}/CBERN.lock");

  if(!$$self{win}) {
  # Not very nice, but it works as /bin/sh in any case...
    system("echo \"$$self{C}/GPS/EXE/makemake.pl -r $$self{C} >$complinkLog 2>&1\"|sh");
    system("echo \"$$self{C}/GPS/EXE/CBERN COMPLINK          >>$complinkLog 2>&1\"|sh");
  }
  else {
    system("perl $$self{C}/GPS/EXE/makemake.pl -r $$self{C} >$complinkLog 2>&1");
    system("perl $$self{C}/GPS/EXE/cbern.pl COMPLINK     >>$complinkLog 2>&1");
  }

  my @FGlist1 = grep { -f $_ } glob("$ENV{FG}/*f");
  my @FGlist2 = grep { -f $_ } glob("$ENV{FG}/*f90");
  my @XGlist1 = grep { -f $_ &&  -x $_ } glob("$ENV{XG}/*");
  my @XGlist2 = grep { -f $_ && !-x $_ && $_ !~ /\.map$/ &&
                                          $_ !~ /\.dwf$/ &&
                                          $_ !~ /\.pdb$/ } glob("$ENV{XG}/*");

  if ( @FGlist1 + @FGlist2 == @XGlist1 - @XGlist2 ) {
    print "\n","*"x70;
    print "\n* Fortran programs compiled successfully.";
    print "\n","*"x70,"\n\n";
  } else {
    my($err)=sprintf("%3d of %3d",@FGlist1+@FGlist2-@XGlist1,@FGlist1+@FGlist2);
    print "###############################################\n";
    print "# Error compiling $err Fortran programs #\n".
          "# Please check the log file...                #\n";
    print "###############################################\n\n";
  }
}

## Example Campaign
###############################################################################
sub configure::_democamp() {
  my $self  = shift;
  my $title = shift;
  if(defined($title) && $title eq "title") {
    my $text = "Install the example campaign";
    $text .= " (only after Step 1)" unless ( defined ($ENV{C}) );
    return $text;
  }

  # Check that campaign directories are available
  # ---------------------------------------------
  if(!defined($ENV{P})) {
    print "Variable \${P} is not defined!\n".
          "Load Bernese environment using 'LOADGPS.setvar'\n".
          "before restarting 'configure.pm' program.\n";
    return;
  }
  if(!defined($ENV{D})) {
    print "Variable \${D} is not defined!\n".
          "Load Bernese environment using 'LOADGPS.setvar'\n".
          "before restarting 'configure.pm' program.\n";
    return;
  }
  if(!defined($ENV{S})) {
    print "Variable \${S} is not defined!\n".
          "Load Bernese environment using 'LOADGPS.setvar'\n".
          "before restarting 'configure.pm' program.\n";
    return;
  }

  # Create campaign directories if they don't exist
  # -----------------------------------------------
  if(!-d $ENV{P} &&
    uc($self->_yesNo("\n".
           "Your campaign area does not exist:\n$ENV{P}\n".
           "Create it now (y/n): ")) eq "Y") {
    File::Path::mkpath("$ENV{P}");
  }
  if(!-d $ENV{D} &&
    uc($self->_yesNo("\n".
           "Your datapool area does not exist:\n$ENV{D}\n".
           "Create it now (y/n): ")) eq "Y") {
    File::Path::mkpath("$ENV{D}");
  }
  if(!-d $ENV{S} &&
    uc($self->_yesNo("\n".
           "Your savedisk area does not exist:\n$ENV{S}\n".
           "Create it now (y/n): ")) eq "Y") {
    File::Path::mkpath("$ENV{S}");
  }

  if(-d $ENV{P} && -d $ENV{D} && -d $ENV{S}) {
    print "\n";

    # Loop all example campaigns
    # --------------------------
    my($filnam0,$filnam1);
    foreach my $demoCamp("CAMPAIGN$$self{VERSION}","DATAPOOL","SAVEDISK") {
      $filnam0 = (!$$self{win}) ? "$$self{C}/GPS/DOC/$demoCamp.tgz" : "$$self{C}/GPS/DOC/$demoCamp.exe";
      $filnam1 = (!$$self{win}) ? "$demoCamp.tar.gz"                : "$demoCamp.exe";

      # Is the archive available?
      # -------------------------
      if(!-e $filnam0) {
        print "\n ### Archive of Example campaign not found: $filnam0\n";
        next;
      }

      # Start extraction
      # ----------------
      print "Extracting Example campaign $demoCamp...\n";
      if($demoCamp =~ /^CAMPAIGN$$self{VERSION}$/) {
        chdir("$ENV{P}");
      }
      elsif($demoCamp =~ /^DATAPOOL$/) {
        chdir("$ENV{D}");
      }
      elsif($demoCamp =~ /^SAVEDISK$/) {
        chdir("$ENV{S}");
      }
      File::Copy::copy($filnam0,$filnam1);
      if(!$$self{win}) {
        system("gzip -dc $filnam1 | tar -xvf -");
      } else {
        system("$filnam1");
      }
      unlink($filnam1);
    }

    print "\nInstallation of the Example campaign has been finished\n\n";
    print "ATTENTION:\n";
    print "When you are going to run them, please make sure that your\n";
    print "user scripts are still not modified.\n";
  }

  # Check for programs
  # ------------------
  my @pgmList = $$self{win} ? ("crx2rnx.exe","gzip.exe") :
                              ("CRX2RNX",    "gzip");
  foreach(@pgmList) {
    warn "\n ### Executable $_ is missing!\n" unless(-x $self->myWhich($_));
  }

}

## Update BSW (ftp archive from Bern)
################################################################################
sub configure::_updatebsw() {
# ------------------------------------------------------------------------------
#
# Purpose:    Tool to help updating BSW5.0 (bugfixes)
#             - intended for UNIX/Linux and Windows.
#             - windows: command line interface to winzip (v9) is required,
#               otherwise users will have to unzip manually.
#             - checks for completeness and plausibility of Bernese installation
#             - warns users if input panels or userscripts are changed
#
# Author:     R.Dach/P.Fridez
#
# Created:    27-Oct-2005 (Adapted from configure.pm)
#
# Changes:    14-Nov-2005   : bugs fixed
#             21-Aug-2008   : /local/ added to path in first line
#             24-Aug-2010 SL: tab characters removed
#             13-Jan-2012 SL: included in configure.pm again
#
# ------------------------------------------------------------------------------

  my $self  = shift;
  my $title = shift;

  if(defined($title) && $title eq "title") {
    my $text = "Install online updates";
    unless ( $$self{win} ) {
      $text .= " (only after Step 1)" unless ( defined ($ENV{C}) );
    }
    return $text;
  }

  # Check that all necessary environment variables are available
  # ------------------------------------------------------------
  foreach my $var(qw /OS X XQ LG FG BPE/) {
    if(!defined($ENV{$var})) {
      if($ENV{OS_NAME} ne "WIN32") {
        print "\n *** Variable \$\{${var}\} is not defined... can't continue!\n".
                " *** Is Bernese environment loaded?\n\n";
        return;
      }
      else {
        print "\n *** Variable \$\{${var}\} is not defined... can't continue!\n".
                " *** Bernese is not correctly installed on your PC.\n\n";
        return;
      }
    }
  }

  # Extract release date from RELEASE.TXT
  # -------------------------------------
  my $relInfo = "$$self{C}/GPS/DOC/RELEASE.TXT";
  # Is file available
  if(! -s $relInfo) {
    print "\n *** Incomplete Bernese installation... can't continue!\n".
            " *** The file $relInfo is missing.\n".
            " *** Reinstall the software from original media and try again.\n\n";
    return;
  }
  else {
    open(TXT,"<$relInfo");
    while(<TXT>) {
      if(/^Release:  ([\d]{4}-[\d]{2}-[\d]{2})$/ ) {
        $relInfo = $1; last;
      }
    }
    close(TXT);
    if(substr($relInfo,4,1) ne "-" ) {
      print "\n *** Release date not found (TXT)... can't continue!\n".
              " *** Reinstall the software from original media and try again.\n\n";
      return;
    }
  }

  my $updTar = "update_${relInfo}.tar.gz";
  $updTar = "update_${relInfo}.zip" if($$self{win});

  # Extract the user name from the Bernese source code
  # --------------------------------------------------
  my $user = "$$self{C}/LIB/FOR/IOR.f";

  # File not found
  if(! -s $user) {
    print " *** Incomplete installation... can't continue!\n".
          " *** File $user missing.\n".
          " *** Reinstall the software from original media and try again.\n\n";
    return;
  }
  # Extract the release date
  else {
    my $ii = 0;
    open(FOR,"< $user");
    while(my $line = <FOR>) {
      $ii++;
      if($ii == 12) {
        $user = lc(substr($line,31,6)); last;
      }
    }
    close(FOR);
  }

  # Extract the password from the Bernese source code
  # --------------------------------------------------
  my $passwd = "$$self{C}/LIB/FOR/TIMST2.f90";

  # file not found
  if(! -s $passwd) {
    print "\n *** Incomplete installation... can't continue!.\n".
            " *** File $passwd missing.\n".
            " *** Reinstall the software from original media and try again.\n\n";
    return;
  }
  # Extract the release date
  else {
    my $ii = 0;
    open(FOR,"< $passwd");
    while(my $line = <FOR>) {
      $ii++;
      if ($ii == 11) {
        chomp($passwd = substr($line,19,10)); last;
      }
    }
    close(FOR);
  }

  # Get the latest archive
  # ----------------------
#########################
#  $user = "";
#  $passwd = "";
#########################

  my $tarFil = $updTar;
  if($$self{win}) { $tarFil = "$$self{C}\\$updTar"; }
  else {            $tarFil = "$$self{C}/$updTar";  }

  if(-e $tarFil) {
    rename($tarFil,"${tarFil}_old");
  }
  # Check which programs are available
  my($wget,$testFile);
  chomp($wget=`which wget`) unless $$self{win};
  my $ilast=0;

  # Try all until the archive is downloaded
  do {

    # At first wget:
    if(! $$self{win} && -e $wget) {

      system("cd $$self{C};".
             "wget -q --http-user=$user --http-passwd=\"$passwd\" ".
              "http://www.bernese.unibe.ch/UPDATE52/${updTar}_notyet");
      if(-e "${tarFil}_notyet") {
        unlink("${tarFil}_notyet");
        print "\n","*"x70;
        print "\n* Your software version is up-to-date.";
        print "\n* No further online updates are available.";
        print "\n","*"x70,"\n\n";
        $ilast=1;
        return;
      }

      system("cd $$self{C};".
             "wget -q --http-user=$user --http-passwd=\"$passwd\" ".
              "http://www.bernese.unibe.ch/UPDATE52/$updTar");
      $wget = "";
      print "\n *** wget failed to get the update file.\n\n" unless(-s $tarFil);
    }


    # Nothing else did work, download manually
    # ----------------------------------------
    if(!-s $tarFil) {
      do {

        print "Installation found in $$self{C}!\n".
              "Environment found!\n\n".
              "------------------\n".
              "Download the file $updTar from:\n\n".
              "   http://www.bernese.unibe.ch/UPDATE52\n".
              "   User name:  $user\n".
              "   Password:   $passwd\n";
        print "\nto $$self{C}.\n";
        print "-------------------\n";

        print "\n";
        print "Please note: IF THE FILE $updTar IS NOT YET AVAILABLE\n";
        print "ON THE DOWNLOAD PAGE, YOUR VERSION IS UP-TO-DATE.\n";

        $self->_userInput("Press the return key when finished...");

        unless (-s $tarFil) { 
          print "\nNo update archive found at $tarFil\n";
          return;
        }

      } until(-s $tarFil);
    }


    # Check the completeness of the archive
    # -------------------------------------
     $testFile="GPS/DOC/update.txt";

  } until($testFile eq "GPS/DOC/update.txt");

  # Where to extract the update
  # ---------------------------
  my $dir = "$$self{C}";

  if($ilast eq 0){
    while(1==1) {
      print "\nUpdated files will be extracted to:\n$dir\n";

      if($dir eq "$$self{C}") {
        print "\nIf you made changes in the Bernese source code, you can specify".
              "\nan alternative directory and merge the updated files manually".
              "\ninto your version to keep your changes.\n\n";
      }

      unless (uc($self->_yesNo(
              "Extract the files to $dir (y/n): ")) eq "Y") {

        $dir = $self->_userInput("Enter alternative directory",$dir);
      } else { last }
    }

  # Create this directory if it does not exist
  # ------------------------------------------
    if(! -d $dir &&
      uc($self->_yesNo("\n".
             "Directory does not exist: $dir\n".
             "Create it now (y/n): ")) eq "Y") {
      File::Path::mkpath($dir);
    }

    unless (-d $dir) {
      print " *** Directory could not be created... will exit now\n";
      return;
    }

  # Save the Perl-path
  # ------------------
    my $perlFile = "$$self{C}/BPE/RUNBPE.pm";
    my $perlPath = "/usr/bin/perl";

  # file not found
    if(!$$self{win}){
      if(!-s $perlFile) {
        print "\n *** Incorrect installation found... exiting\n".
                " *** The file $perlFile is missing.\n".
                " *** Reinstall the software before the update step.\n\n";
        return;
      } else{
        open(BPE,"< $perlFile");
        while(<BPE>) {
          chomp($perlPath = $_); last;
        }
        close(BPE);

        $perlPath =~ s/^#![ ]*//;
        until($perlPath =~ /perl$/ ) { $perlPath = substr($perlPath,0,-1) }

      # Does the Perl really exist?
        unless(-x $perlPath) {
          print "*** The path to Perl was extracted from ${perlFile}.\n".
                "*** The Perl interpreter cannot be found at this location.\n";
          until(-x $perlPath) {
            $perlPath = $self->_userInput("Please enter the path to perl",$perlPath);
          }
        }
      }
    }

  # Extract the archive
  # -------------------
    if(!$$self{win}) { my $pwd = `pwd` };

#    chomp($pwd);

    chdir($dir);
    my @inpFil = ();
    my @scrFil = ();

    if(!$$self{win}) {
      print"\n";
      open(TAR,"gzip -dc $$self{C}/$updTar | tar -xvf - |");
    } else {
      print"\n";
      print" Please select the unzip utility available on your system:\n";
#
# Windows users: here^s a list of a few commonly used zip-programs:
#  "unzip" : the unix port
#  "7-zip" : free windows compression utility from www-7-zip.org
#  "winzip": winzip but WITH commandline interface (wzunzip)
#  if you wish: add your own uncompress utility. It must be able to
#  handle zip files, to test them and to extract them
#
### Here's the selection list:
      print " 1 = unzip\n";
      print " 2 = 7-zip\n";
      print " 3 = winzip including command line interface\n\n";
      print "Enter selection (1,2,3): ";
      my $sel = <STDIN>;

### Here's the actual code for each utility:
## 1) unzip:
      if($sel == "1")
      { unless ( system("unzip -t $$self{C}\\$updTar")==0 ) {
         print "could not unzip, do it manually: file $$self{C}\\$updTar\n"; return }
        unless ( open(TAR,"unzip $$self{C}\\$updTar |")) { print "cannot extract"; return }
      }
## 2) 7z:
      elsif ($sel == "2")
      {  unless ( system("7z t  $$self{C}\\$updTar")==0 ) {
           print "could not unzip, do it manually: file $$self{C}\\$updTar\n"; return }
         unless ( open(TAR,"7z x $$self{C}\\$updTar |")) { print "cannot extract"; return }
      }
## 3) wzunzip:
      elsif ($sel == "3")
      { unless ( system("wzunzip -t $$self{C}\\$updTar")==0 ) {
          print "could not unzip, do it manually: file $$self{C}\\$updTar\n".
               "command line extension of winzip not found \n";return}
        unless( open(TAR,"wzunzip -do $$self{C}\\$updTar |") ) { print "cannot extract"; return}
      }else{
## none worked..
      print " not able to extract update file, do it manually\n";
      }
## Finished extracting the files
    }

    while(my $line = <TAR>) {
      print "$line" unless($line =~ /\/$/);
      if (!$$self{win}) {
       if ($line =~ /.*GPS\/PAN\/([\w]*)\.INP.*/) {
         push @inpFil,$1
       }
       if ($line =~ /.*GPS\/USERSCPT\/([\w]*).*/) {
         push @scrFil,$1
       }
      } else{
        if ($line =~ /.*GPS\\PAN\\([\w]*)\.INP.*/) {
          push @inpFil,$1
        }
        if ($line =~ /.*GPS\\USERSCPT\\([\w]*).*/) {
          push @scrFil,$1
        }
      }
    }
    close(TAR);

  # Perl-update
  # -----------
    $self->change_shebang($perlPath,$dir);

  # Give the instructions to finish the update
  # ------------------------------------------

    print"\n\n****************************\n";
    print "Source code update completed.\n";
    if($dir ne "$$self{C}") {
      print "Merge the new source code in $dir\n".
            "into your version in $$self{C}.\n";
    }
    print "****************************\n\n";
    print "Recompile the software using \$\{X\}/EXE/configure.pm, or\n".
          "run the Perl script \"\$\{X\}/EXE/cbern.pl COMPLINK\"\n\n";

    if(@scrFil) {
      print "BPE user scripts are modified by this update!\n".
            "Copy (or merge if necessary) them into the \$\{U\}/SCRIPT directory of \n".
            "all users. List of scripts:\n";

      while (@scrFil) {
        printf "   %-10s  %-10s  %-10s  %-10s  %-10s\n",splice(@scrFil,0,5);
      }
    }

    if(@inpFil && -e "$ENV{U}/PAN/UPDPAN.INP") {
      unless (-x "$ENV{XG}/PUTKEYW" &&
              (-x "$ENV{XQ}/menu" || -x "$ENV{XQ}/menu.exe")) {
      print "Input panels are modified by this update!\n".
            "A panel update using  \"menu - Configure - Update input files\" is  \n".
            "necessary for changes to take effect! \n",
            "This must be done for all users and in all BPE option directories\n".
            "Options to be used:\n\n".
            " \"Panel directory\":  \$\{U\}/PAN    and   \$\{U\}/OPT/*\n".
            " \"UPDATE OPTIONS\" :  UPDATE     resp. EXISTING\n\n".
            "Select the following file(s) as \"MASTER PROGRAM INPUT FILES\":\n";
      map { print "                        $_\n" } @inpFil;
      } else {
  # create file with names
      my $selFil = "$ENV{T}/updpan.tmp";
      open OUT,">$selFil";
      map { s/\n$// } @inpFil;
      map { print OUT "$_\n" } @inpFil;
      close OUT;
      if($$self{win}) {
        system("$ENV{XG}/PUTKEYW $ENV{U}/PAN/UPDPAN.INP OLDPAN REPLACE $selFil");
      } else {
        system("echo $ENV{U}/PAN/UPDPAN.INP OLDPAN REPLACE $selFil|$ENV{XG}/PUTKEYW");
      }
      unlink $selFil;
      system("$ENV{C}/GPS/EXE/RUNGPS UPDPAN");
      }
    }
  }
}

# ------------------------------------------------------------------------------
sub configure::myWhich() {
# ------------------------------------------------------------------------------
  my $self   = shift;
  my $pgmNam = shift;
  my $found  = "";

  if(-e $pgmNam) {
    $found = $pgmNam;
  }
  elsif($pgmNam !~ /\// && $pgmNam !~ /\\/) {
    my(@pathList) = $ENV{PATH} =~ /;/ ?
                    split(";",$ENV{PATH}) :
                    split(":",$ENV{PATH});
    foreach my $path(@pathList) {
      my($file) = $path . "/" . $pgmNam;
      if(-e $file ) {
        $found = $file;
        last;
      }
    }
  }
  return($found);
}

# ==============================================================================
1;
