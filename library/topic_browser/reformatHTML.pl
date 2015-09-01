use strict;
use Encode;
use Data::Dumper;

my $topic_word_file = $ARGV[0];
my $ytr_topic_word_file = $ARGV[1];
my $doc_prop_file = $ARGV[2];
my $raw_text_file = $ARGV[3];
my $output_folder = $ARGV[4];
my $wdt_file = $ARGV[5];
my $param_string = $ARGV[6];

my @topic_colors = ('rgba(166,206,227,0.9)','rgba(31,120,180,0.9)','rgba(178,223,138,0.9)','rgba(51,160,44,0.9)','rgba(251,154,153,0.9)','rgba(227,26,28,0.9)','rgba(253,191,111,0.9)','rgba(255,127,0,0.9)','rgba(202,178,214,0.9)','rgba(106,61,154,0.9)','rgba(255,255,153,0.9)','rgba(177,89,40,0.9)','rgba(166,206,227,0.625)','rgba(31,120,180,0.625)','rgba(178,223,138,0.625)','rgba(51,160,44,0.625)','rgba(251,154,153,0.625)','rgba(227,26,28,0.625)','rgba(253,191,111,0.625)','rgba(255,127,0,0.625)','rgba(202,178,214,0.625)','rgba(106,61,154,0.625)','rgba(255,255,153,0.625)','rgba(177,89,40,0.625)','rgba(166,206,227,0.35)','rgba(31,120,180,0.35)','rgba(178,223,138,0.35)','rgba(51,160,44,0.35)','rgba(251,154,153,0.35)','rgba(227,26,28,0.35)','rgba(253,191,111,0.35)','rgba(255,127,0,0.35)','rgba(202,178,214,0.35)','rgba(106,61,154,0.35)','rgba(255,255,153,0.35)','rgba(177,89,40,0.35)');


print STDERR "Calling updateDocCount ", Dumper(\@ARGV);

open (FILE, $topic_word_file) or die $!;
open (PROPFILE, $doc_prop_file) or die $!;

open (FILEYR, $ytr_topic_word_file) or die $!;

open (RAWTXT, $raw_text_file) or die $!;

open (TOPICOUT1, "+>$output_folder" . '/topics.html') or die $!;

my @rawArr = <RAWTXT>;
my $rawtext = join("",@rawArr);
close RAWTXT;

my $outputArr;
my $lastTopic;
my $k=-1;
my $lineno=0;
my $outputHash;
while (my $line = <FILE>) {
	if ($lineno == 0) {
		$lineno++;
		next;
	}
	chop($line);
	my @lineArr = split(/,/,$line);
	if ($lastTopic ne $lineArr[0]) {
		$k++;
	}
	$lastTopic = $lineArr[0];
	push(@{$outputHash->{$lineArr[0]}}, $lineArr[1] . " [" . $lineArr[2] . "]");

}

$lineno=0;
my $propHash_doc;
my $propHash;

my @fldArr;
while (my $line = <PROPFILE>) {
	chop($line);
	if ($lineno == 0) {
		$lineno++;
		$line =~ s/\"//sg;
		@fldArr = split(/,/,$line);
		next;
	}
	my @lineArr = split(/,/,$line);
	my %hash;
	@hash{ @fldArr } = @lineArr;
	push(@{$propHash_doc->{$hash{doc}}}, \%hash);
	push(@{$propHash->{$hash{topic}}->{$hash{factor}}}, \%hash);
	
}

close PROPFILE;

my $yr_hash;
my $topicNum;	
my $first_line = <FILEYR>;
chomp($first_line);
my @yrs = split(/,/,$first_line);
while (my $line = <FILEYR>) {
	#print STDERR "file yr line [$line]\n";
	if ($line =~ /^Topic: (\d+)/) {
		$topicNum = $1;
	}
	if ($line =~ /^"/) {
		my @tmpArr;
		eval '@tmpArr = (' . $line . ');'; 
		push(@{$yr_hash->{$topicNum}}, \@tmpArr); 
	}
}


close FILE;
close FILEYR;
my $k=0;
foreach my $ky (sort { $a <=> $b } keys(%$outputHash) ) {
	#print "Adding elem $k [$ky] ", Dumper( $outputHash->{$ky} );
	$outputArr->[$k] = $outputHash->{$ky};
	$k++;
}

# create the menu entries for each factor level
my $select_list = join('</option><option>', @yrs);
print STDERR $select_list;

my $navform = "<form><select onChange='yr=this.value;if (yr==
\"All factors\") {window.location.href=\"topics.html\";} else { window.location.href=\"topics_\" + yr + \".html\";}'><option>Select a subcorpus</option><option>All factors</option><option>$select_list</option></select></form>
Click on a topic header to look at articles for a topic";

print TOPICOUT1 "<html><body><h3>Topics for Full Corpus</h3><h5>Source file: $topic_word_file",	"$ytr_topic_word_file<br/>Date Generated ",`date`, "Method:Standard LDA using R-LDA<br/>$param_string</h5>", 
	"$navform<table border=1><tr>";

my $th_open = "<th style=\"background-color:bgcolor\"><a href=\"topic_x_y.html\">Topic ";
my $th_close = "</a></th>";

	my $y=0;
	my $done=0;
	for (my $x=0;$x<$k;$x++) {

	my $tmp = $th_open;
	my $newX = $x+1;
	$tmp =~ s/_x/_$newX/;
	my $newY = "full";
	$tmp =~ s/_y/_$newY/;
	my $bgcolor = $topic_colors[$x];
	$tmp =~ s/bgcolor/$bgcolor/;
	
	print TOPICOUT1 $tmp, $x+1, $th_close;
	}
	print TOPICOUT1 "</tr><tr>";
	for (my $x=0;$x<$k;$x++) {
		print TOPICOUT1 "<td>";
		for (my $y=0; $y<scalar(@{$outputArr->[0]});$y++) {
			my $tmp = $outputArr->[$x]->[$y];
			$tmp =~ s/\[\d.*//sg;
			print TOPICOUT1 "$tmp<br/>";
		}
		print TOPICOUT1 "</td>";
	}
	
print TOPICOUT1 "</tr></table></body></html>";
close TOPICOUT1;

for my $yrelem (0..$#yrs) {
	my $yr = @yrs[$yrelem];
	
	my @topics = keys(%$yr_hash);
	
	open (TOPICOUT2, "+>$output_folder" . '/topics_' . $yr . '.html') or die $!;
	print TOPICOUT2 "<html><body><h3>Topics for $yr Corpus</h3><h5>Source file: ",
	"$ytr_topic_word_file<br/>Date Generated ",`date`, "Method:Standard LDA using R-LDA<br/>$param_string</h5>", 
	"$navform<table border=1><tr>";
	for (my $x=0;$x<scalar(@topics);$x++) {
		my $tmp = $th_open;
		my $newX = $x+1;
		$tmp =~ s/_x/_$newX/;
		my $newY = $yr;
		$tmp =~ s/_y/_$newY/;
		my $bgcolor = $topic_colors[$x];
		$tmp =~ s/bgcolor/$bgcolor/;
	
		print TOPICOUT2 $tmp, $x+1,$th_close;
	}
	print TOPICOUT2 "</tr><tr>";
	
	for (my $x=0;$x<=scalar(@topics)-1;$x++) {
		print TOPICOUT2 "<td>";
		foreach my $row (@{$yr_hash->{$x}}) {
			my $tmp = $row->[$yrelem];
			$tmp =~ s/\[\d.*//sg;
			print TOPICOUT2 "$tmp<br/>";
		}
		print TOPICOUT2 "</td>";
	}
	print TOPICOUT2 "</tr></table></body></html>";
	close TOPICOUT2;
}



my @topics = keys(%$yr_hash);
for my $yrelem (0..$#yrs+1) {
	my $yr = "full";
	if ($yrelem <= $#yrs) {
       $yr = @yrs[$yrelem];
	}
	for (my $x=0;$x<=scalar(@topics)-1;$x++) {	
		print STDERR "yr is $yr\n";
		my $newfile = $output_folder . '/topic_' . ($x+1) . "_" . $yr . '.html';
		print STDERR "New file is [$newfile]\n";
		open (TOPICOUT2, "+>$newfile") or die $!;
		print TOPICOUT2 "<html><body><h3>Topic " . ($x+1) . " Articles for $yr Corpus</h3><h5>Source file: $ytr_topic_word_file</h5><table border=1><tr><th>Article</th><th>Document Proportion</th><th>Manager</th><th>Factor</th></tr>";
		my @rows;
		if ($yr ne "full") {
			if (exists $propHash->{$x}->{$yr}) {
				@rows = @{$propHash->{$x}->{$yr}};
			}
		} else {
			foreach my $tmpyr (keys %{$propHash->{$x}}) {
				foreach my $tmprow (@{$propHash->{$x}->{$tmpyr}}) {
					push(@rows,$tmprow);
				}
			}
		}
		
		@rows =  sort { $b->{docprop} <=> $a->{docprop} } @rows;
		foreach my $row (@rows) {
			#print data on topic
			#print STDERR "row", Dumper($row);
			print TOPICOUT2 "<tr><td><a href=\"article_", $row->{doc}, ".html
			\">", $row->{doc},"</a></td><td>", $row->{docprop}, "</td><td>",$row->{source}, "</td><td>",$row->{factor},"</td></tr>";
		}
		
		print TOPICOUT2 "</table></body></html>";
		close TOPICOUT2;
	}
	if ($yr eq "full") {
			last;
	}
}
my $recId = 1;
open (WDTFILE, $wdt_file) or die $!;
my $wdt_line = <WDTFILE>;
$wdt_line = <WDTFILE>;
while ($rawtext =~ m/=== RECORD (\d+) ===(.*?)=== END RECORD ===/sg) {
	open (TOPICOUT2, "+>$output_folder" . '/article_' . $1 . '.html') or die $!;
	print TOPICOUT2 "<html><body><h3>Text for Article $1 </h3><table border=1><tr><th>Topic</th><th>Proportion</th></tr>";
	
	my @rows = @{$propHash_doc->{$1}};
	@rows =  sort { $a->{topic} <=> $b->{topic} } @rows;
	foreach my $row (@rows) {
		print TOPICOUT2 "<tr><td style=\"background-color:", $topic_colors[$row->{topic}], "\">", ($row->{topic}+1), "</td><td>", $row->{docprop}, "</td></tr>";
	}
	
	my $rec_num = $1;
	my $rec_text = $2;
	if ($rec_num % 500==0) {print STDERR "printing article $rec_num\n"};
	my $newtext = undef;
	if ($rec_text =~ m/^.*?Corpus Text:.*?\b/s) {
		$newtext = $&;
	}
	my $left_text;
	while ($wdt_line =~ /^$rec_num\,/) {
		chop($wdt_line);
		my @wdt_line_arr = split(/,/,$wdt_line);
		my $match_text = $wdt_line_arr[3];
		$match_text =~ s/\"//g;
		
		if ($match_text eq "national_endowment_arts") {
			$match_text = 'national endowment.*?arts';
		}
		my $match_topic = $wdt_line_arr[4];
		my $spancolor = $topic_colors[$match_topic];
		#print STDERR "looking to peel [$newtext]\n";
		#print STDERR "==ltext is [$left_text]\n";
		#print STDERR "==rtext is [$rec_text]\n";
			
		if (defined($newtext)) {
			#print STDERR "match found\n";
			$left_text .= $newtext;
			$rec_text = substr($rec_text, length($newtext));
		}
		#print STDERR "==trying match on string [$match_text] [$match_topic]\n";
		if ($rec_text =~ s/^(.{0,100}?\b)($match_text)(\b)/$1\<span style="background-color:$spancolor">$2<\/span>$3/isg) {
			$newtext = "$1<span style=\"background-color:$spancolor\">$2</span>$3";
			#print STDERR "updating new text to [$newtext]\n";
		} else {
			$newtext = undef;
		}
	
		$wdt_line = <WDTFILE>;
	}
	print TOPICOUT2 "</table><p style=\"white-space:pre-wrap;\">$left_text$rec_text</p>";
	$recId++;
	print TOPICOUT2 "</body></html>";
	close TOPICOUT2;
}

close WDTFILE;