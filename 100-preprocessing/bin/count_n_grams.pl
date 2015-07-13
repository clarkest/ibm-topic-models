my $max = 20;

my @documents;

my %term_counts = ();
my %doc_counts = ();

my $doc = 0;
while (<>) {
    chomp;
    
    ## if there are multiple columns get rid of all but the last
    my @fields = split /\t/, $_;

    my @tokens = split /\s+/, $fields[16];
    foreach my $token (@tokens) {
	$term_counts{0}{$token}++;
	$doc_counts{0}{$doc}++;
    }

    push @documents, \@tokens;
    $doc++;
}

for (my $window = 1; $window <= $max; $window++) {
    print STDERR "$window\t";

    my $count = 0;

    for ($doc = 0; $doc < scalar @documents; $doc++) {
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

if (defined $term_counts{$max}) {
    my %max_counts = %{$term_counts{$max}};
    foreach my $term (keys %max_counts) {
	print "$max_counts{$term}\t$term\n";
    }
}
