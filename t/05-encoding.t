use strict;
use warnings FATAL => 'all';

use Test::More;
use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Test::DZil;
use Test::Deep;
use Test::Fatal;
use Path::Tiny;
use utf8;

use Test::Requires { 'Dist::Zilla' => '5.000' };

binmode $_, ':encoding(UTF-8)' foreach map { Test::Builder->new->$_ } qw(output failure_output todo_output);
binmode STDOUT, ':encoding(UTF-8)';
binmode STDERR, ':encoding(UTF-8)';

my $tzil = Builder->from_config(
    { dist_root => 't/does-not-exist' },
    {
        add_files => {
            path(qw(source dist.ini)) => simple_ini(
                [ GatherDir => ],
                [ MetaConfig => ],
                [ 'AuthorityFromModule' => { module => 'Foo::ಠ_ಠ' } ],
            ),
            path(qw(source lib Foo.pm)) => "use strict;\nuse warnings;\npackage Foo;\nuse utf8;\npackage Foo::ಠ_ಠ;\n1;\n",
        },
    },
);

$tzil->chrome->logger->set_debug(1);
is(
    exception { $tzil->build },
    undef,
    'build proceeds normally',
);

cmp_deeply(
    $tzil->distmeta,
    superhashof({
        x_authority_from_module => "Foo::\x{ca0}_\x{ca0}",
        x_permissions_from_module => "Foo::\x{ca0}_\x{ca0}",
        x_Dist_Zilla => superhashof({
            plugins => supersetof(
                {
                    class => 'Dist::Zilla::Plugin::AuthorityFromModule',
                    config => {
                        'Dist::Zilla::Plugin::AuthorityFromModule' => {
                            module => "Foo::\x{ca0}_\x{ca0}",
                        },
                    },
                    name => 'AuthorityFromModule',
                    version => ignore,
                },
            ),
        }),
    }),
    'plugin metadata got the package encoding correct',
) or diag 'got distmeta: ', explain $tzil->distmeta;

diag 'got log messages: ', explain $tzil->log_messages
    if not Test::Builder->new->is_passing;

done_testing;
