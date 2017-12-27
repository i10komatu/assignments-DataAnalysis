#!/usr/bin/perl
use strict;
use warnings;


my $dimension=300;

open(IN,"wikispeedia_paths-and-graph/categories.tsv") or die;
my %category;
while(my $line=<IN>){
	chomp $line;
	next if($line=~/^#/);
	if($line=~/(.+)\s+(.+)/){
		unless (defined $category{$1}){
			my @categories=split(/\./,$2);
			$category{$1}=$categories[1];
		}
	}
}
close(IN);

open(IN,"stopwords.txt") or die;
my %stop;
while(my $line=<IN>){
	chomp $line;
	$stop{$line}=1;
}
close(IN);

my %all_article_word_count;
my $dirname="plaintext_articles";
opendir(DIR,$dirname) or die;
my @files=readdir(DIR);
closedir(DIR);

foreach my $file(@files){
	next unless ($file=~/^(.+?)\.txt$/);
	next unless (defined $category{$1});
	open(IN,"$dirname/$file") or die;

	while(my $line=<IN>){
		chomp $line;
		my @array=split(/\s+/,$line);
		foreach (@array){
			next if($_ eq "");
			next if (defined $stop{lc($_)});
			$all_article_word_count{lc($_)}++;
		}
	}
	close(IN);
}


my @word_order=sort {$all_article_word_count{$b}<=>$all_article_word_count{$a}} keys %all_article_word_count;

open(OUT,">bow.txt");
foreach my $file (@files){
	next unless ($file=~/(.+)?\.txt/);
	next unless (defined $category{$1});
	print OUT $1," ",$category{$1};
	my %hash;
	open(IN,"$dirname/$file") or die;
	while(my $line=<IN>){
		chomp $line;
		my @array=split(/\s+/,$line);
		foreach (@array){
			next if (defined $stop{lc($_)});
			next if($_ eq "");
			$hash{lc($_)}++;
		}
	}
	close(IN);
	my $count=0;
	foreach my $w (@word_order){
		if(defined $hash{$w}){
			print OUT " $hash{$w}";
		}
		else{
			print OUT " 0";
		}
		$count++;
		last if($count>$dimension);
	}
	print OUT "\n";
}

close(OUT);


system "R --vanilla --slave <bow_clust.R";
