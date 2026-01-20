# Fibonacci (Perl)

use strict;
use warnings;

sub fib {
  my ($n) = @_;
  return $n if $n < 2;
  return fib($n - 1) + fib($n - 2);
}

for my $i (0..11) {
  print "$i ", fib($i), "\n";
}
