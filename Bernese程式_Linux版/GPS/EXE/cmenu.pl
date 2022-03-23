#!/usr/bin/perl -w
# ==============================================================================
#
# Name:      cmenu.pl
#
# Author:    S.Lutz
#
# Created:   16-Nov-2010
#
# Changes:   15-Dec-2010 SL: consider QTBERN if available
#            11-Jan-2012 SL: use cmp_util istead of gpsutil
#            13-Jan-2012 SL: &copy instead of cp, pattern for TARGET changed
#            16-Jan-2012 SL/MM: backup previous menu executable
#                            use system cp to prevent attributes
#            27-Feb-2012 SL: bugfix if menu does not exist
#            30-Mar-2012 SL: qmake call within cmenu_private subroutine
#            04-Apr-2012 SL: check for trailing blanks in check4tab call
#            19-May-2012 SL: use utime instead of system cp -p
#            27-Aug-2012 SL: Use &os from cmp_util
#            05-Sep-2012 SL: Use &myWhich from bpe_util for (n)make
#            13-Nov-2012 EO: Adapt for Mac OS X
#
# Programs:  make, qmake
#
# ==============================================================================

use strict;
use File::Basename qw(basename);
use File::Compare qw(compare);
use File::Copy qw(copy);
use File::Path qw(mkpath);

use lib "$ENV{X}/EXE";
use cmp_util qw(os check4tab filter_make);

# Show help
# ---------
sub help {
  die "\n".
  "NAME:     ".&basename($0)."\n\n".
  "PURPOSE:  Compile Bernese Menu.\n\n".
  "SYNOPSIS: ".&basename($0)." [OPTIONS] [name]\n\n".
  "OPTIONS:  -h       Print this help message\n".
  "          -n       No execution mode, print only commands\n".
  "          -all     Compile all files\n".
  "          name     Name to compile\n\n".
  "EXAMPLES: ".&basename($0)."\n".
  "          ".&basename($0)." -all >\$XQ/MENUCOMP.log 2>&1 &\n\n".
  "";
}

&help if(defined($ARGV[0]) && $ARGV[0] eq "-h");

# Get Menu
# --------
my($menuRoot) = "$ENV{XQ}";
my(%os)      = &os;

# Check for tab characters
# ------------------------
unlink("$menuRoot/qrc_menu.cpp");  # File with trailing blanks
&check4tab("-b",glob("$menuRoot$os{sl}*.{h,c,cpp,sh,pl}")) && exit;

# Standard options
# ----------------
my $optMake  = "";

# Explicit name
# -------------
my $name = $ARGV[-1] && $ARGV[-1] !~ /^-/ ? pop @ARGV : "";

# Special make and makemake options
# ---------------------------------
my $optStr   = join " ",@ARGV;
   $optMake .= $optStr =~ s/-n//   ? " -n" : "" ;
my $optAll  .= $optStr =~ s/-all// ? " -B" : "" ;

# Make make file
# --------------
chdir($menuRoot);
open(PRO,"<menu.pro");
open(TMP,">menu.tmp");
while(<PRO>) {
  $_ =~ s/^TARGET\s*=\s*menu/TARGET = menu_tmp/g;
  print TMP $_;
}
close(TMP);
close(PRO);

my($qmake) = "";
$qmake = "$ENV{QTDIR}/bin/qmake$os{exe_suffix}" if($ENV{QTDIR});
$qmake = "$ENV{QTBERN}/bin/qmake$os{exe_suffix}" if($ENV{QTBERN});
die "$! ($qmake)\n" unless(-x $qmake);

# Make command
# ------------
sub cmenu_private {

  use Sys::Hostname qw(hostname);
  use lib "$ENV{BPE}";
  use bpe_util qw(myWhich);

# Print header
# ------------
  print "\n\n","="x80;
  print "\nCompiling the menu";
  print "\n$ENV{USER}\@".&hostname." (".$^O.")";
  print "\nXQ = $ENV{XQ}";
  print "\n".localtime(time);
  print "\n","="x80,"\n\n";

# Make
# ----
  print "menu.pro -> menu.tmp\n\n";
  system("$qmake --version");
  system("$qmake -Wall menu.tmp");
  chdir($menuRoot);
  if(-x &myWhich("make")) {
    print("\nmake $optMake $optAll\n\n");
    system("make -i $optMake $optAll 2>&1");
  }
  elsif(-x &myWhich("nmake$os{exe_suffix}")) {
    print("\nnmake$os{exe_suffix} $optMake $optAll\n\n");
    system("nmake$os{exe_suffix} /i $optMake $optAll 2>&1");
  }
  else {
    die "\n(n)make is missing!\n\n";
  }

# Print footer
# ------------
  print "\n","="x80,"\n";
  print "Compilation finished\n";
}

# Filter output
# -------------
&filter_make unless($ENV{OS} =~ /win/i);
&cmenu_private();

# Backcopy of menu_tmp
# --------------------
chdir($menuRoot);
my($mkmenu);

open(MKF,"<Makefile");
while(<MKF>) {
    chomp;
    $mkmenu = (split(/\s*Makefile for building:\s*/))[1];
    last if($mkmenu);
}
close(MKF);

# Mac OS X
if ( $mkmenu eq 'menu_tmp.app/Contents/MacOS/menu_tmp' ) {

  my $mac_dir  = 'menu_tmp.app/Contents/MacOS';
  my $menu     = "$mac_dir/menu";
  my $menu_bck = "$mac_dir/menu.bck";
  my $menu_tmp = "$mac_dir/menu_tmp";

  if ( -x $menu_tmp
       && ( !-e $menu || ( stat($menu_tmp) )[9] > ( stat($menu) )[9] ) )
  {
    ### Backup existing menu before replacing it with the new one
    if (-e $menu) {
      copy($menu, $menu_bck)
        or die "fatal: copy ($menu -> $menu_bck) failed. $!";
      print "existing menu backed up (copy $menu -> $menu_bck)\n";
    }

    ### Replace existing menu with the newly created one
    copy($menu_tmp, $menu)
      or die "fatal: copy ($menu_tmp -> $menu) failed. $!";
    print "Old menu replaced with the new one (copy $menu_tmp -> $menu)\n";
    chmod 0755, $menu
      or die "fatal: could not change permission to menu ($menu). $!";

    ### Add symbolink link to $XQ
    unlink "menu" if -f "menu";
    symlink $menu, "menu"
      or die "fatal: could not create menu symbolic link. $!";
    print "symlink added menu -> $menu\n";

    unlink "menu_tmp" if -f "menu_tmp";
    symlink $menu_tmp, "menu_tmp"
      or die "fatal: could not create menu_tmp symbolic link. $!";
    print "symlink added menu_tmp -> $menu_tmp\n";

    unlink("menu.tmp");
  }

  ### Copy the qt_menu.nib directory
  my $QtDir;
  $QtDir = $ENV{QTDIR}  if defined $ENV{QTDIR};
  $QtDir = $ENV{QTBERN} if defined $ENV{QTBERN};
  die "Qt installation directory should be available from environment variables"
    unless defined $QtDir;
  die "Directory ($QtDir) not found" unless -d $QtDir;

  if (-d "$QtDir/qt_menu.nib") {
    print "qt_menu.nib directory found in $QtDir\n";
    my $nibDir = "$menuRoot/menu_tmp.app/Contents/Resources/qt_menu.nib";
    unless (-d $nibDir) {
      mkpath("$nibDir") or die "Cannot create directory ($nibDir). $!";
    }

    map{ copy("$QtDir/qt_menu.nib/$_", "$nibDir") or die "Cannot copy ($_). $!";
      print "   File $_ copied to $nibDir.\n"}
    ("classes.nib", "info.nib", "keyedobjects.nib");
  } else {
    my $banner = "=" x 55;
    print "\n\n$banner\n";
    print "WARNING!\n";
    print "qt_menu.nib directory not found in $QtDir!\n";
    print "Should have been copied after the installation of Qt.\n";
    print "Please refer to \${X}/DOC/README_INSTALL.TXT.\n";
    print "$banner\n\n";
  }
}
# UNIX/Win
else {
  unless(&compare($mkmenu,"menu_tmp$os{exe_suffix}") == 0) {
    &copy($mkmenu,"menu_tmp$os{exe_suffix}");
  }
  if(-x "menu_tmp$os{exe_suffix}" &&
    (!-e "menu$os{exe_suffix}" ||
     (stat("menu_tmp$os{exe_suffix}"))[9] > (stat("menu$os{exe_suffix}"))[9])
  ) {
    if(-e "menu$os{exe_suffix}") {
      &copy("menu$os{exe_suffix}", "menu$os{exe_suffix}.bck") &&
        print "menu$os{exe_suffix} -> menu$os{exe_suffix}.bck\n";
      utime(time(),(stat("menu$os{exe_suffix}"))[9],"menu$os{exe_suffix}.bck");
    }
    &copy("menu_tmp$os{exe_suffix}", "menu$os{exe_suffix}") &&
      print "menu_tmp$os{exe_suffix} -> menu$os{exe_suffix}\n";
    chmod((stat("menu_tmp$os{exe_suffix}"))[2],"menu$os{exe_suffix}");
    utime(time(),(stat("menu_tmp$os{exe_suffix}"))[9],"menu$os{exe_suffix}");
    unlink("menu.tmp");
    print "\n";
  }
}

close(STDOUT);

# ==============================================================================

__END__
