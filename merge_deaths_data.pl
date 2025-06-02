#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;          # gives us ‘say’
no  autovivification;
binmode STDOUT, ':utf8';
use utf8;

# ── CPAN modules ───────────────────────────────────────────────────────────
use Text::CSV 1.99 ();      # fast CSV parser (XS if available)
use HTTP::Tiny;
use JSON::XS;
use Data::Printer;
use File::Spec;             # portable paths & tmp dir

my %data;  # $data{country}{year}{age_group_5}{deaths}
           # $data{country}{year}{age_group_5}{population}
my %stats;

my $deaths_file = 'data/estat_demo_magec_filtered_en.csv';
my $pop_file    = 'data/estat_demo_pjangroup_filtered_en.csv';

load_eurostats_file($deaths_file);
load_eurostats_file($pop_file);
print_2011_2023_data();
print_1998_2023_data();

sub load_eurostats_file {
    my ($file) = @_;

    my $csv = Text::CSV->new({
        binary      => 1,
        auto_diag   => 1,
        decode_utf8 => 1,
        sep_char    => ',',
    });

    open my $fh, '<:encoding(UTF-8)', $file
        or die "Cannot open '$file': $!";

    my $header = $csv->getline($fh);   # discard header

    while (my $row = $csv->getline($fh)) {
        my ($structure, $structure_id, $structure_name,
            $freq,  $time_frequency,
            $unit, $unit_of_measure,
            $sex, $sex_class,
            $age, $age_class,
            $geo, $geo_entity,
            $time_period, $time,
            $obs_value, $value,
            $flag, $stats
        ) = @$row;

        # Removes useless age_class.
        next if $age_class eq 'Total';
        next if $age_class eq 'Unknown';
        next if $age_class eq '75 years or over';
        next if $age_class eq '80 years or over';

        # Removes useless geo_entity.
        next if $geo_entity eq 'United Kingdom';
        next if $geo_entity eq 'Ukraine';
        next if $geo_entity eq 'Metropolitan France';
        next if $geo_entity eq 'Germany including former GDR';
        next if $geo_entity eq 'Andorra';
        next if $geo_entity eq 'Armenia';
        next if $geo_entity eq 'Georgia';
        next if $geo_entity eq 'Lithuania';
        next if $geo_entity eq 'Monaco';
        next if $geo_entity eq 'Russia';
        next if $geo_entity eq 'San Marino';
        next if $geo_entity eq 'United Kingdom';
        next if $geo_entity eq 'Serbia';
        next if $geo_entity eq 'Romania';
        next if $geo_entity eq 'Albania';
        next if $geo_entity eq 'Azerbaijan';
        next if $geo_entity eq 'Belarus';
        next if $geo_entity eq 'Bulgaria';
        next if $geo_entity eq 'Türkiye';
        next if $geo_entity eq 'Kosovo*';
        next if $geo_entity eq 'Liechtenstein';
        next if $geo_entity eq 'Luxembourg';
        next if $geo_entity eq 'Malta';
        next if $geo_entity eq 'Moldova';
        next if $geo_entity eq 'Montenegro';
        next if $geo_entity eq 'Bosnia and Herzegovina';
        next if $geo_entity eq 'North Macedonia';

        if ($file eq $deaths_file) { # Deaths specific treatments
            # Concatenates by age groups.
            if ($age_class eq 'Less than 1 year') {
                $age_class = 0;
            } elsif ($age_class eq '1 year') {
                $age_class = 1;
            } else {
                $age_class =~ s/ years//;
            }
            $age_class = age_group_5_from_age($age_class);
            $data{$geo_entity}{$time_period}{$age_class}{deaths} += $obs_value;
        } else {
            # Reformats age groups.
            if ($age_class eq '85 years or over') {
                $age_class = '85+';
            } elsif ($age_class eq 'Less than 5 years') {
                $age_class = '0-4';
            } else {
                my ($from, $to) = $age_class =~ /From (.*) to (.*) years/;
                $age_class = "$from-$to";
            }
            die "{$geo_entity}{$time_period}{$age_class}" if exists $data{$geo_entity}{$time_period}{$age_class}{population};
            $data{$geo_entity}{$time_period}{$age_class}{population} = $obs_value;
        }
    }
    close $fh;
}

sub age_group_5_from_age {
    my $age = shift;

    # Top-coded bucket
    return '85+' if $age eq 'Open-ended age class';
    return '85+' if $age >= 85;

    # Everything else falls into a 5-year band
    my $lower = int($age / 5) * 5;
    my $upper = $lower + 4;
    return "$lower-$upper";
}

sub print_2011_2023_data {
    open my $out, '>', 'data/pop_deaths_eurostats_2011_2023.csv';
    say $out "country,year,age_group_5,deaths,population";
    for my $country (sort keys %data) {
        for my $year (sort{$a <=> $b} keys %{$data{$country}}) {
            next if $year < 2011;
            next if $year == '2024';
            for my $age_group_5 (sort keys %{$data{$country}->{$year}}) {
                my $deaths = $data{$country}{$year}{$age_group_5}{deaths} // die "{$country}{$year}{$age_group_5}";
                my $population = $data{$country}{$year}{$age_group_5}{population} // die;
                say $out "$country,$year,$age_group_5,$deaths,$population";
                $stats{'2011-2023'}->{'country'}->{$country}++;
                $stats{'2011-2023'}->{'age_group_5'}->{$age_group_5}++;
                $stats{'2011-2023'}->{'year'}->{$year}++;
            }
        }
    }
    close $out;
}

sub print_1998_2023_data {
    open my $out, '>', 'data/pop_deaths_eurostats_1998_2023.csv';
    say $out "country,year,age_group_5,deaths,population";
    for my $country (sort keys %data) {
        next if $country eq 'Croatia';
        next if $country eq 'Latvia';
        for my $year (sort{$a <=> $b} keys %{$data{$country}}) {
            next if $year == '2024';
            for my $age_group_5 (sort keys %{$data{$country}->{$year}}) {
                my $deaths = $data{$country}{$year}{$age_group_5}{deaths} // die "{$country}{$year}{$age_group_5}";
                my $population = $data{$country}{$year}{$age_group_5}{population} // die "{$country}{$year}{$age_group_5}";
                say $out "$country,$year,$age_group_5,$deaths,$population";
                $stats{'1998-2023'}->{'country'}->{$country}++;
                $stats{'1998-2023'}->{'age_group_5'}->{$age_group_5}++;
                $stats{'1998-2023'}->{'year'}->{$year}++;
            }
        }
    }
    close $out;
}

p%stats;