#!/usr/bin/perl -w

use strict;
use warnings;

use constant TRUE  => 1;
use constant FALSE => 0;

use File::Basename qw/basename dirname/;
use File::Spec::Functions qw/catfile/;
use File::Path qw/mkpath rmtree/;

use LWP::UserAgent;

our $VERSION = "0.1";
my $progname = basename($0);

my $myip = '172.28.12.71';
my $proj = ($ARGV[0] || '');
my $target = ($ARGV[1] || '');
my $maxpage = 100;
my $today = ($ARGV[2] || '');
my $logdir = $today . '_' . $target;
$logdir =~ s#/#_#g;
$logdir .= '_weblog';

if ((@ARGV != 3) || (length($today) != 8) || ($today !~ /\d{8}/)) {
    print "$progname project target date(20151215)\n";
    exit 1;
}

my $ua = LWP::UserAgent->new;
for (my $i = 0; $i < $maxpage; $i++) {

    # リストの取得
    my %param = (
        'target'   => $target,
        'dispmode' => 'log',
        'page'     => $i
    );
    my $res = $ua->post("http://mango/cgi-bin/". $proj . "/ajax_list.cgi", \%param);
    my $content = $res->content;

    my @proclist = split(/;/, $content);
    exit 0 unless ($proclist[1]);
    my @quelist = split(/,/, $proclist[1]);
    exit 0 if (@quelist == 0);

    mkpath($logdir) if ( !-d $logdir );

    foreach my $list (@quelist) {
        my @queline = split(/\|/, $list);
        my $logfile = ($queline[0] || '');
        my $module  = ($queline[1] || '');
        my $cpu     = ($queline[2] || '');
        my $ip      = ($queline[4] || '');

        my ($logdate, $dummy) = split(/!/, $logfile);
        my $date = substr($logdate, 0, 8);

        next if (($today+0) < ($date+0));
        exit 0 if (($date+0) < ($today+0));
        next unless ($myip eq $ip);

        print $list, "\n";
        print 'date='.$date, "\n";
        print 'page='. ($i+1), "\n";
        print 'logfile='.$logfile, "\nmodule=".$module,
            "\ncpu=".$cpu, "\nip=".$ip, "\n";

        # ログの取得
        my %param2 = (
            'target'  => $target,
            'logfile' => $logfile,
        );
        my $logres = $ua->post("http://mango/cgi-bin/" . $proj . "/showlog.cgi", \%param2);

        # ファイル名称作成
        my $name;
        $name = 'fw_' if ($module =~ m/fw/);
        $name = 'navi_' if ($module =~ m/navi/);
        my $file = catfile($logdir, $logdate . '_' . $cpu . '_' . ($name || '') . basename($module). '.html');

        # ファイルへ書き込み
        print $file, "\n";
        open my $out, '>', $file
          or die 'open error: ' . $file . ': ', $!;
        print $out $logres->content;
        close $out;
    }
}
