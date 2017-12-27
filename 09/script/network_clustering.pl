#!/usr/bin/perl
use strict;
use warnings;

open(IN,"wikispeedia_paths-and-graph/categories.tsv") or die;
my %hash;
my $count=0;
open(OUT,">category_hash.txt");
while(my $line=<IN>){
	chomp $line;
	next if($line=~/^#/);
	if($line=~/(.+)\s+(.+)/){
		unless (defined $hash{$1}){
			$hash{$1}=$count++;
			my @categories=split(/\./,$2);
			print OUT $categories[1],"\n";

		}
	}
}
close(OUT);
close(IN);
open(OUT,">network_hash.txt");
open(IN,"wikispeedia_paths-and-graph/links.tsv") or die;
while(my $line=<IN>){
	chomp $line;
	next if($line=~/^#/);
	if($line=~/(.+)\s+(.+)/){
		next unless (defined $hash{$1});
		next unless (defined $hash{$2});
		print OUT $hash{$1}," ",$hash{$2},"\n"
	}
}
close(OUT);
close(IN);

system "R --vanilla --slave <net_clust.R";
