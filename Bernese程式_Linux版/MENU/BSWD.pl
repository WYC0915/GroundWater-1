#!/usr/bin/perl -w

use strict;
use IO::Socket;
use Net::hostent;
use File::stat;
use Fcntl ':mode';
use File::Copy;

use lib "$ENV{gtools}/local_perl/lib/perl5/site_perl/5.8.0/sun4-solaris";
use Compress::Zlib;

# Global Variables
# ----------------
my $LASTLINE   = ".\n";
my $EOM        = "\n" . $LASTLINE;
my $msg_accept = "CONNECTION ACCEPTED";
my $msg_error  = "ERROR";
my $client;

# Expand environment variables
##############################################################################
sub expandEnv {
  my($string) = @_;
  while ( $string =~ /(\$\{)(\w+)(\})/ ) {
    my $hlp = eval '$ENV{$2}';
    $string =~ s/(\$\{)(\w+)(\})/$hlp/;
  }
  return $string;
}

# Read the Environment
##############################################################################
sub readEnv {
  my ($cmd)  = @_;
  my $retval = "";
  foreach my $key ( split(" ", $cmd) ) {
    $retval .= "$key = $ENV{$key}\n";
  }
  return $retval;
}

# Read the content of a file
##############################################################################
sub readFile {
  my ($fileName) = @_;
  $fileName = expandEnv($fileName);
  if (! -s $fileName || !open(inFile,  "<$fileName")) {
    return "";
  }
  my @lines = <inFile>;
  close(inFile);
  return join("",@lines);
}

# Write the file
##############################################################################
sub writeFile {
  my ($fileName, $concLine) = @_;
  $fileName = expandEnv($fileName);
  if (!open(outFile,  ">$fileName")) {
    return "N";
  }
  print outFile $concLine;
  close(outFile);
  return "Y";
}

# Remove the file
##############################################################################
sub removeFile {
  my ($fileName) = @_;
  $fileName = expandEnv($fileName);
  if ( unlink($fileName) == 1) {
    return "Y";
  }
  else {
    return "N";
  }
}

# Read the content of a directory
##############################################################################
sub lstDir {
  my ($dirName) = @_;
  my $filter;

  $dirName = expandEnv($dirName);

  if ($dirName =~ /([^\?]+)\?(.*)$/) {
    if ($2 eq "exist") {
      if ( -r $1) {
        return "Y";
      }
      else {
        return "N";
      }
    }
    if ($2 eq "size") {
      my $sb = stat($1);
      return $sb->size;
    }
    else {
      $dirName = $1;
      if (defined $2) {
        $filter = $2;
      }
      else {
        $filter = "*";
      }
    }
  }

  if ( chdir($dirName) ) {

    # directories
    # -----------
    my @dirNames = ();
    my @hlp = glob("*");
    for (my $i = 0; $i < @hlp; $i++) {
      if (-d $hlp[$i]) {
        my $sb    = stat( $hlp[$i] );
        my $rwx   = ($sb->mode & S_IRWXU) >> 6;
        @dirNames = (@dirNames, "$hlp[$i] $rwx d");
      }
    }

    # files
    # -----
    my @fileNames = glob($filter);
    for (my $i = 0; $i < @fileNames; $i++) {
      if ( -d $fileNames[$i] ) {
        $fileNames[$i] = "";
        next;
      }
      if ( ! -r $fileNames[$i] ) {
        $fileNames[$i] = "";
        next;
      }
      my $sb    = stat( $fileNames[$i] );
      my $rwx   = ($sb->mode & S_IRWXU) >> 6;
      $fileNames[$i] .= " $rwx f";
    }

    return join("\n", @dirNames, @fileNames);
  }
  else {
    return "";
  }
}

# Run a program
##############################################################################
sub runPgm {
  my ($command) = @_;
  if ( system($command) ) {
    return "N";
  }
  else {
    return "Y";
  }
}

# Process the Buffer
##############################################################################
sub processBuffer {

  my $command  = Compress::Zlib::uncompress(shift);
  my $concLine = "";

  my $ind = index($command, "\n");
  if ($ind != -1) {
    $concLine = substr($command, $ind+1, length($command));
    $command  = substr($command, 0, $ind);
  }

  my $ans;
  if    ($command =~ /^\s*GET\s+(.*)\s*$/) {
    my $cmd = $1;
    if ($cmd =~ /\?ENVIRONMENT(.*)/) {
      $ans = readEnv($1);
    }
    else {
      $ans = readFile($cmd);
    }
  }
  elsif ($command =~ /^\s*DIR\s+(.*)\s*$/) {
    $ans = lstDir($1);
  }
  elsif ($command =~ /^\s*PUT\s+(.*)\s*$/) {
    $ans = writeFile($1, $concLine);
  }
  elsif ($command =~ /^\s*REM\s+(.*)\s*$/) {
    $ans = removeFile($1);
  }
  elsif ($command =~ /^\s*RUN\s+(.*)\s*$/) {
    $ans = runPgm($1);
  }
  elsif ($command =~ /^\s*COPY\s+(.*)\s+(.*)\s*$/) {
    File::Copy::copy($1, $2);
    $ans = "Y";
  }
  elsif ($command =~ /^\s*MKDIR\s+(.*)\s*$/) {
    mkdir(expandEnv($1));
    $ans = "Y";
  }
  elsif ($command =~ /^\s*QUIT\s*/)    {
    print "QUIT\n";
    close $client;
    exit;
  }
  else {
    $ans = $msg_error;
  }

##beg test
  print "Command: $command\n";
  if ($command !~ /^\s*GET\s+(.*)\s*$/) {
    print "answer = $ans\n";
  }
##end test

  print $client Compress::Zlib::compress($ans) . $EOM;
  $command  = undef;
  $concLine = "";
}

# Main program
##############################################################################

my $PORT = shift || 1967;


# Create the server
# -----------------
my $server = IO::Socket::INET->new(Proto     => 'tcp',
                                   LocalPort => $PORT,
                                   Listen    => SOMAXCONN,
                                   Reuse     => 1);

die "cannot setup server" unless $server;

# "Listen" loop
# -------------
while ($client = $server->accept()) {

  binmode $client;
  $client->autoflush(1);

  my $hostinfo = gethostbyaddr($client->peeraddr);
  printf("Connect from %s\n", $hostinfo->name || $client->peerhost);
  print $client Compress::Zlib::compress($msg_accept) . $EOM;

  # Fork
  # ----
  my $pid = fork; die "cannot fork" unless defined($pid);

  # The child does the work
  # -----------------------
  if (!$pid) {
    my $command  = undef;
    my $concLine = "";


    # Read as many bytes as possible
    # ------------------------------
    my $buffer = "";
    while (1) {
      my $data;
      my $dataLen = sysread($client, $data, 100000);

      if ($dataLen > 0) {
        $buffer .= $data;

        my $bufLen = length($buffer);
        if ($bufLen >= 3               &&
          substr($buffer, $bufLen-3, 3) eq "\n.\n") {
          $buffer = substr($buffer, 0, $bufLen-3);
          processBuffer($buffer);
          $buffer = "";
        }
      }
    }
  }
}

