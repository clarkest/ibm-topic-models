binmode STDOUT, ":utf8";
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


while (<>) {
    chomp;
    
    my @fields = split /\t/, $_;
    my $text = $fields[17];
    my $title = $fields[12];

    for my $i (0..$#source) {
       $text =~ s/\b$source[$i]\b/$target[$i]/gi;
       $title =~ s/\b$source[$i]\b/$target[$i]/gi;
    }
    print join("\t", @fields) . "\n";
    #printf("%d\t%s\n", $fields[0], $text);
}
