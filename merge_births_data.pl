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

my %data;  # $data{country}{year}{age_group_5}{births}
           # $data{country}{year}{age_group_5}{population}
my %stats;

my $births_file = 'data/estat_demo_fordagec_defaultview_filtered_en.csv';
my $pop_file    = 'data/estat_demo_pjangroup_filtered_en_female.csv';

load_eurostats_births_file($births_file);
load_eurostats_population_file($pop_file);
print_2011_2023_data();

sub load_eurostats_births_file {
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
            $age, $age_class,
            $ord, $birth_order,
            $geo, $geo_entity,
            $time_period, $time,
            $obs_value, $value,
            $flag, $stats
        ) = @$row;

        # Removes useless age_class.
        next if $age_class eq 'Total';
        next if $age_class eq 'Unknown';

        # Removes useless time_period.
        next if $time_period < 2015;
        next if $time_period == '2024';

        # Removes useless geo_entity.
        next if $geo_entity eq 'Euro area - 19 countries  (2015-2022)';
        next if $geo_entity eq 'Euro area – 20 countries (from 2023)';
        next if $geo_entity eq 'European Economic Area (EU27 - 2007-2013 and IS, LI, NO)';
        next if $geo_entity eq 'European Economic Area (EU28 - 2013-2020 and IS, LI, NO)';
        next if $geo_entity eq 'European Free Trade Association';
        next if $geo_entity eq 'European Union - 27 countries (from 2020)';
        next if $geo_entity eq 'European Union - 27 countries (2007-2013)';
        next if $geo_entity eq 'European Union - 28 countries (2013-2020)';
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
        next if $geo_entity eq 'Germany';
        next if $geo_entity eq 'San Marino';
        next if $geo_entity eq 'United Kingdom';
        next if $geo_entity eq 'Serbia';
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

        # Concatenates by age groups.
        $age_class =~ s/ years//;
        $age_class = age_group_5_from_age($age_class);

        $stats{'births'}->{'geo_entity'}->{$geo_entity}++;
        $stats{'births'}->{'age_class'}->{$age_class}++;
        $stats{'births'}->{'time_period'}->{$time_period}++;
        $data{$geo_entity}{$time_period}{$age_class}{births} += $obs_value;
    }
    close $fh;
}

sub load_eurostats_population_file {
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
        next if $geo_entity eq 'Germany';
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

        # Reformats age groups.
        if ($age_class eq '85 years or over') {
        	next;
        } elsif ($age_class eq 'Less than 5 years') {
        	next;
        } else {
            my ($from, $to) = $age_class =~ /From (.*) to (.*) years/;
            next if $from < 15 || $from > 45;
            $age_class = "$from-$to";
        }
        die "{$geo_entity}{$time_period}{$age_class}" if exists $data{$geo_entity}{$time_period}{$age_class}{population};

        # Removes useless time_period.
        next if $time_period < 2015;
        next if $time_period == '2024';

        $stats{'population'}->{'geo_entity'}->{$geo_entity}++;
        $stats{'population'}->{'age_class'}->{$age_class}++;
        $stats{'population'}->{'time_period'}->{$time_period}++;
        $stats{'population'}->{'sex_class'}->{$sex_class}++;
        $data{$geo_entity}{$time_period}{$age_class}{population} = $obs_value;
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
    open my $out, '>', 'data/pop_births_eurostats_2011_2023.csv';
    say $out "country,year,age_group_5,births,population";
    for my $country (sort keys %data) {
        for my $year (sort{$a <=> $b} keys %{$data{$country}}) {
            for my $age_group_5 (sort keys %{$data{$country}->{$year}}) {
                my $births = $data{$country}{$year}{$age_group_5}{births} // die "{$country}{$year}{$age_group_5}";
                my $population = $data{$country}{$year}{$age_group_5}{population} // die "{$country}{$year}{$age_group_5}";
                say $out "$country,$year,$age_group_5,$births,$population";
                $stats{'2011-2023'}->{'country'}->{$country}++;
                $stats{'2011-2023'}->{'age_group_5'}->{$age_group_5}++;
                $stats{'2011-2023'}->{'year'}->{$year}++;
            }
        }
    }
    close $out;
}

p%stats;