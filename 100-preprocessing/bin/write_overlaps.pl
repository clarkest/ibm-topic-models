use utf8;
my $max = 9;

my @documents;

my %term_counts = ();
my %doc_counts = ();

my $doc = 0;

open IN, shift @ARGV;


while (<IN>) {
    chomp;
    
    ## text is in the 18th column
    my @fields = split /\t/, $_;

    my @tokens = split /\s+/, $fields[17];
    foreach my $token (@tokens) {
		$term_counts{0}{$token}++;
		$doc_counts{0}{$doc}++;
    }

    push @documents, \@tokens;
    $doc++;
}
close IN;

# windows: start with next adjacent token, so 0 is the current unigram

for (my $window = 1; $window <= $max; $window++) {
    print STDERR "$window\t";

    my $count = 0;

    for ($doc = 0; $doc < scalar @documents; $doc++) {

	## if a document has no N-1 grams, it has no N grams
	if (! defined $doc_counts{$window - 1}{$doc}) { next; }

	my $arrayref = $documents[$doc];
	my @tokens = @{$arrayref};

	for (my $i = 0; $i < (scalar @tokens) - $window; $i++) {
	    if ($term_counts{0}{$tokens[$i]} < 2) { next; }

	    ## Check the internal n-grams
	    my $is_rejected = 0;
	    for (my $j = 1; $j < $window; $j++) {
		my $term = join " ", @tokens[$i..($i + $j)];
		if ($term_counts{$j}{$term} < 2) { $is_rejected = 1; last; }
	    }

	    if ($is_rejected == 0) {
		my $term = join " ", @tokens[$i..($i + $window)];
		$term_counts{$window}{$term}++;
		$doc_counts{$window}{$doc}++;
		$count++;
	    }
	}
    }

    my $distinct_docs = scalar keys(%{$doc_counts{$window}});
    print STDERR "$count $distinct_docs\n";
    if ($count == 0) { last; }
}

my $window = $max;

my %shared_sequences = ();

foreach $doc (keys %{$doc_counts{$window}}) {
    
    my $arrayref = $documents[$doc];
    my @tokens = @{$arrayref};
    
    my $sequence_start = -1;

    for (my $i = 0; $i < (scalar @tokens) - $window; $i++) {
		## Ignore hapax legomena
		if ($term_counts{0}{$tokens[$i]} < 2) {
		    next;
		}
		
		## Check the internal n-grams
		my $is_rejected = 0;

		for (my $j = 1; $j < $window; $j++) {
		    my $term = join " ", @tokens[$i..($i + $j)];

		    if ($term_counts{$j}{$term} < 2) {
			if ($sequence_start != -1) {
			    
			    my $length = $i + $j - 1 - $sequence_start;
			    if ($length > 15) {
				my $term = join(" ", @tokens[$sequence_start..($i + $j - 1)]);
				$shared_sequences{$term}++;
			    }
			    $sequence_start = -1;
			}
			$is_rejected = 1;
			last;
		    }
		}

		if ($is_rejected == 0) {
		    
		}
		
		if ($is_rejected == 0 && $sequence_start == -1) {
		    $sequence_start = $i;
		}
    }

    if ($sequence_start != -1) {
		my $term = join(" ", @tokens[$sequence_start..(scalar(@tokens) - 1)]);
		$shared_sequences{$term}++;
    }
}

foreach my $term (keys %shared_sequences) {
    printf("%d\t%s\n", $shared_sequences{$term}, $term);
}
