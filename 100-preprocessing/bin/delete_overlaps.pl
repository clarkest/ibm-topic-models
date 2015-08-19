use utf8;
my $doc = 0;

my %terms = ();
open TERMS, shift @ARGV;

while (<TERMS>) {
    chomp;
    my ($count, $term) = split /\t/, $_;
    $terms{$term} = 0;
}
close TERMS;

open IN, shift @ARGV;

## first line
my $first_line = <IN>;
print $first_line;

while (<IN>) {
    ## text is in the 17th column
    my @fields = split /\t/, $_;
    my $text = $fields[17];
    
    $text =~ s/\s+/ /g;
    
    foreach my $term (keys %terms) {
	if ($text =~ /\Q$term\E/) {
	    if ($terms{$term} > 0) { 
		  $text =~ s/\Q$term\E/ /;
	    }
	    $terms{$term}++;
	}
    }

    if (length $text > 50) {
	   $fields[17] = $text;
	   print join("\t", @fields);
    }
    else {
        # output the id of any document being killed off -- this will
        # be necessary to making sure that the nongrams and ngram corpuses
        # have the same doc set
        print STDERR "$fields[18]" . "\n"
    }
    

    # periodically throw a counter to stderr so that the user doesn't get worried
    #$doc++;
    #if ($doc % 1000 == 0) { print STDERR "$doc\n"; }
}
close IN;
