#!/usr/local/bin/perl -w

use strict;
use IO::Socket;

my $EOM = "\n.\n";

unless (@ARGV > 2) { die "usage: $0 host port command" }

my ($host, $port, @command) = @ARGV;

my $server = IO::Socket::INET->new(Proto => "tcp",
                            PeerAddr  => $host,
                            PeerPort  => $port)
       or die "can't connect to port $port on $host: $!";

$server->autoflush(1);

print "Connected to $host:$port\n";

print $server join(" ", @command) . $EOM;

print $server "PUT test.txt\n"         .
              "This is an example\n"   .
              "of data sent to server" . $EOM;

print $server "QUIT" . $EOM;

while ( my $line = <$server> ) {
  print "Server response:\n>$line<";
}
