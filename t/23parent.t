test_data() for @empty_array;

{
    no warnings;

    sub test_data {
        BB() if AA();
        DD() for CC();
        my $x = 10;
        FF() while EE() < --$x;
        for ( my $y; $y; ++$y ) {
            ++$x;
        }

        0 for 0;
    }
}

use Test::More skip_all =>
    "->parent doesn't work on children of optimized-away opnodes";
use B 'svref_2object';
use B::Utils 'walkoptree_simple';

# use B::Concise;
# B::Concise::set_style("#hyphseq2 (*(   (x( ;)x))*)#exname #class=(#addr) #arg ~#flags(?(/#private)?)(x(;~->#next)x)\n",
# 		      "  (*(    )*)     goto #seq\n",
# 		      "(?(<#seq>)?)#exname#arg(?([#targarglife])?)");
# B::Concise::compile( "test_data" )->();

# Set the # of tests to run and make a table of parents
my $tests = 0;
my $root  = svref_2object( \&test_data )->ROOT;
walkoptree_simple( $root, sub { ++$tests } );

# plan( tests => $tests );

walkoptree_simple(
    $root,
    sub {
        my $op = shift;

        ### Op: $op->stringify
        ### Next: eval { $op->next->stringify }
        ### First: eval { $op->first->stringify }
        ### First-next: eval { $op->first->next->stringify }

        my $parent = $op->parent;
        ### Parent: eval { $parent->stringify }
        if ( $$op == $$root ) {
            is( $parent, undef, "No parent for root " . $op->stringify );
        }
        else {
            ok( $parent, "Parent for " . $op->stringify );
        }
    }
);
