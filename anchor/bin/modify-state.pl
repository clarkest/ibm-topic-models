use strict;

my %word_ids = ();
my @vocabulary = ();

my %word_weights = ();
my %word_sums = ();

open IN, shift @ARGV;
while (<IN>) {
    chomp;
    $word_ids{$_} = scalar @vocabulary;
    push @vocabulary, $_;
}
close IN;

my $row = 0;
open IN, shift @ARGV;
while (<IN>) {
    my $word = $vocabulary[$row];
    chomp;
    my @probabilities = split /\t/, $_;

    my $sum = 0.0;
    foreach my $p (@probabilities) {
	$sum += $p;
    }

    $word_weights{$word} = \@probabilities;
    $word_sums{$word} = $sum;
    $row++;
}
close IN;

while (<STDIN>) {
    if (/^#/) {
	print;
	next;
    }

    chomp;
    my ($doc, $na, $pos, $word_id, $word, $topic) = split / /, $_;

    if (defined $word_ids{$word}) {
	if (! defined $word_weights{$word}) { print STDERR "missing $word\n"; }

	my @weights = @{$word_weights{$word}};
	my $sample = $word_sums{$word} * rand();
	$topic = 0;
	while ($sample > $weights[$topic]) {
	    $sample -= $weights[$topic];
	    $topic++;

	    if ($topic > scalar @weights) { print STDERR "Overran on $word\n"; last; }
	}
    }
    printf("%d NA %d %d %s %d\n", $doc, $pos, $word_id, $word, $topic);
}
