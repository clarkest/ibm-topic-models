binmode STDOUT, ":utf8";
binmode STDIN, ":utf8";
#my %abbrev_replacements = ();
my @source = ();
my @target = ();

open REPS, "lists/abbrevs.csv";
while (<REPS>) {
    chomp;
    my @fields = split /,/, $_;
    #my $source = $fields[0];
    #my $destination = $fields[1];
    #$abbrev_replacements{$source} = $destination;
    #print "$source to $destination\n";
    push (@source, $fields[0]);
    push (@target, $fields[1]);
}
close REPS;

#my %plur_replacements = ();
open REPS, "lists/plurals.csv";
while (<REPS>) {
    chomp;
    my @fields = split /,/, $_;
    #my $source = $fields[1];
    #my $destination = $fields[0];
    #$plur_replacements{$source} = $destination;
    push (@source, $fields[1]);
    push (@target, $fields[0]);
    #print "$source to $destination\n";
}
close REPS;

#my %phrase_replacements = ();
open REPS, "lists/new_phrases.txt";
while (<REPS>) {
    chomp;
    # my $source = $_;
    my $destination = $_;
    $destination =~ s/ /_/g;

    #$phrase_replacements{$source} = $destination;
    push (@source, $_);
    push (@target, $destination);
    #print "$source to $destination\n";
}
close REPS;

my $doc=0;
while (<>) {
    chomp;
    
    my @fields = split /\t/, $_;
    my $title = $fields[0];
    $title =~ s/->//g; 
    $title =~ s/[>,<]//g;  
    $title =~ s/\h+/ /g;
    $title =~ s/"/'/g;
    $title =~ s/(\p{L})[\xe2]\p{P}+(\p{L})/$1'$2/g;
    $title =~ s/[\xe2]\W+/ /g;

    for my $i (0..$#source) {
       $title =~ s/\b$source[$i]\b/$target[$i]/gi;
    }
    print $fields[0] . "\t" . $title . "\n";
    #printf("%d\t%s\n", $fields[0], $text);
    #periodically throw a counter to stderr so that the user doesn't get worried
    $doc++;
    if ($doc % 1000 == 0) { print STDERR "$doc\n"; }
}
