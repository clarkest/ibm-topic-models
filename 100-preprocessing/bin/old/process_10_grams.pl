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

# we also need to undo the no-ngrams phrases.txt
my @phrase = ();
my @old_ngram = ();
open REPS, "lists/phrases.txt";
while (<REPS>) {
    chomp;
    # my $source = $_;
    my $un_tokened = $_;
    $un_tokened =~ s/_/ /g;

    #$phrase_replacements{$source} = $destination;
    push (@phrase, $_);
    push (@old_ngram, $un_tokened);
    #print "$source to $destination\n";
}
close REPS;

while (<>) {
    chomp;
    
    my @fields = split /\t/, $_;
    my $text = $fields[1];

    for my $i (0..$#phrase) {
       $text =~ s/\b$phrase[$i]\b/$old_ngram[$i]/gi;
    }
    for my $i (0..$#source) {
       $text =~ s/\b$source[$i]\b/$target[$i]/gi;
    }

    printf("%d\t%s\n", $fields[0], $text);
}
