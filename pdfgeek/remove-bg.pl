#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long qw(:config bundling no_auto_abbrev);
use File::Basename;
use File::Glob ':glob';
use IPC::Run3;
use File::Path qw(make_path remove_tree);

my $input_file;
my $pages;
my $threshold = 40;

GetOptions(
    'threshold=i' => \$threshold,
) or die "Usage: $0 <input_file> <pages> [--threshold=40]\n";

$input_file = shift @ARGV if @ARGV;
$pages      = shift @ARGV if @ARGV;

die "Missing input_file\n" unless defined $input_file && -f $input_file;
die "Missing pages (e.g., '1,3-5')\n" unless defined $pages;

my $filename_only = basename($input_file);
$filename_only =~ s/\.[^.]+$//;
my $base_name_only = $filename_only;
my $metadata_title = $base_name_only . '_Cleaned';

my $base_name_cleaned = $input_file;
$base_name_cleaned =~ s/\.[^.]+$//i;
$base_name_cleaned .= '_cleaned.pdf';

print "Cleaning: $filename_only\n";
print "Metadata will be set to: $metadata_title\n";

sub run_cmd {
    my ($cmd) = @_;
    my $err;
    IPC::Run3::run3($cmd, \undef, \undef, \$err);
    if ($err && $err ne '') {
        print "Error running command: $cmd\n$err\n";
        exit 1;
    }
}

my $output_dir = 'final_pdf_pages';

# 1. Cleanup & Setup
remove_tree($output_dir) if -d $output_dir;
make_path($output_dir);

# 2. Parse Pages - properly convert to integers
my @page_list;
foreach my $part (split /,/, $pages) {
    $part =~ s/^\s+|\s+$//g;  # trim whitespace
    if ($part =~ m{-}) {
        my ($start, $end) = split /-/, $part;
        push @page_list, $start .. $end;
    } else {
        push @page_list, $part;
    }
}

# 3. Process Pages
foreach my $p (@page_list) {
    my $padded = sprintf '%03d', $p;
    print "Processing Page $p (Threshold: $threshold%)\n";

    # Ghostscript: Extract page to PNG
    run_cmd(qq{gswin64c -sDEVICE=pnggray -r300 -dFirstPage=$p -dLastPage=$p -o temp_page.png -q -dBATCH -dNOPAUSE "$input_file"});

    # ImageMagick: Apply threshold
    run_cmd(qq{magick temp_page.png -white-threshold $threshold% -colorspace gray -type bilevel -compress fax temp_cleaned.png});

    # Tesseract: OCR to PDF
    run_cmd(qq{tesseract temp_cleaned.png $output_dir/page_$padded -l eng pdf});
}

# 4. Merge
print "Merging final PDF...\n";

open my $pages_fh, '>', 'pages.txt' or die "Cannot create pages.txt: $!";
foreach my $pdf (sort glob "$output_dir/page_*.pdf") {
    my $pdf_fwd = $pdf;
    $pdf_fwd =~ tr{\\}{/};
    print $pages_fh "\"$pdf_fwd\"\n";
}
close $pages_fh;

# Create Metadata
open my $docinfo_fh, '>', 'docinfo.ps' or die "Cannot create docinfo.ps: $!";
print $docinfo_fh "[ /Title ($metadata_title) /Author (YSB-LLM) /DOCINFO pdfmark\n";
close $docinfo_fh;

run_cmd(qq{gswin64c -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dQUIET -sOutputFile="$base_name_cleaned" \@pages.txt docinfo.ps});

# Final Cleanup
foreach my $f (qw(temp_page.png temp_cleaned.png pages.txt docinfo.ps)) {
    unlink $f if -e $f;
}

print "Done! Created $base_name_cleaned\n";
