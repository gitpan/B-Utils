package B::Utils;

use 5.006;
use strict;
use vars qw( $VERSION @EXPORT_OK %EXPORT_TAGS
    @bad_stashes $TRACE_FH $File $Line $Sub );

use subs (
    qw( all_starts all_roots anon_sub recalc_sub_cache ),
    qw( walkoptree_simple walkoptree_filtered ),
    qw( walkallops_simple walkallops_filtered ),
    qw( opgrep ),
    qw( croak carp )
);

use Scalar::Util qw( weaken );

# use Smart::Comments;

=pod

=head1 NAME

B::Utils - Helper functions for op tree manipulation

=head1 VERSION

0.05_01 - This is a dev version and
  is part of an effort to add tests,
  functionality, and merge a fork
  from Module::Info.

=cut

$VERSION = 0.05_01;

=head1 SYNOPSIS

  use B::Utils;

=head1 DESCRIPTION

These functions make it easier to manipulate the op tree.

=head1 FUNCTIONS

=cut

use B qw( OPf_KIDS main_start main_root walksymtable class main_cv ppname );

use Exporter ();
@EXPORT_OK = qw(all_starts all_roots anon_subs
    walkoptree_simple walkoptree_filtered
    walkallops_simple walkallops_filtered
    recalc_sub_cache
    opgrep );
%EXPORT_TAGS = ( all => \@EXPORT_OK );
*import      = \&Exporter::import;

@bad_stashes
    = qw(B Carp Exporter warnings Cwd Config CORE blib strict DynaLoader vars XSLoader AutoLoader base);

BEGIN {

    # Fake up a TRACE constant and set $TRACE_FH
    BEGIN { $^W = 0 }
    eval 'sub TRACE () {' . ( 0 + $ENV{B_UTILS_TRACE} ) . '}';
    die $@ if $@;
    $TRACE_FH ||= \*STDOUT;
}
sub TRUE ()  { !!1 }
sub FALSE () { !!0 }

=pod

=over 4

=item C<all_starts>

=item C<all_roots>

Returns a hash of all of the starting ops or root ops of optrees, keyed
to subroutine name; the optree for main program is simply keyed to C<__MAIN__>.

B<Note>: Certain "dangerous" stashes are not scanned for subroutines:
the list of such stashes can be found in C<@B::Utils::bad_stashes>. Feel
free to examine and/or modify this to suit your needs. The intention is
that a simple program which uses no modules other than C<B> and
C<B::Utils> would show no addition symbols.

This does B<not> return the details of ops in anonymous subroutines
compiled at compile time. For instance, given

    $a = sub { ... };

the subroutine will not appear in the hash. This is just as well, since
they're anonymous... If you want to get at them, use...

=cut

my ( %starts, %roots );
sub all_starts { _init_sub_cache(); wantarray ? %starts : \%starts }
sub all_roots  { _init_sub_cache(); wantarray ? %roots  : \%roots }

=pod

=item C<anon_subs()>

This returns an array of hash references. Each element has the keys
"start" and "root". These are the starting and root ops of all of the
anonymous subroutines in the program.

=cut

my @anon_subs;
sub anon_subs { _init_sub_cache(); wantarray ? @anon_subs : \@anon_subs }

=pod

=item C< recalc_sub_cache() >

If PL_sub_generation has changed or you have some other reason to want
to force the re-examination of the optrees, everywhere, call this
function.

=cut

my $subs_cached = FALSE;

sub recalc_sub_cache {
    $subs_cached = FALSE;

    %starts = %roots = @anon_subs = ();

    _init_sub_cache();
    return;
}

sub _init_sub_cache {

    # Allow this function to be run only once.
    return if $subs_cached;

    %starts = ( __MAIN__ => main_start() );
    %roots  = ( __MAIN__ => main_root() );

    # Through the magic of B::'s ugly callback system, %starts and
    # %roots will be populated.
    walksymtable(
        \%main::,
        _B_Utils_init_sub_cache => sub {

            # Do not eat our own children!
            $_[0] eq "$_\::" && return FALSE for @bad_stashes;

            return TRUE;
        },
        ''
    );

    # Some sort of file-scoped anonymous code refs are found here. In
    # general, when a function has anonymous functions, they can be
    # found in the scratchpad.
    push @anon_subs,
        map( (
            'CV' eq class($_)
            ? { root  => $_->ROOT,
                start => $_->START
                }
            : ()
        ),
        main_cv()->PADLIST->ARRAY->ARRAY );

    $subs_cached = TRUE;
    return;
}

sub B::GV::_B_Utils_init_sub_cache {

    # This is a callback function called from B::Utils::_init via
    # B::walksymtable.

    my $gv = $_[0];
    my $cv = $gv->CV;

    # If the B::CV object is a pointer to nothing, ignore it.
    return unless $$cv;

    # Simon was originally using $gv->SAFENAME but I don't think
    # that's a "correct" decision because then oddly named functions
    # can't be disambiguated. If a function were actually named ^G, I
    # couldn't tell it apart from one named after the control
    # character ^G.
    my $name = $gv->STASH->NAME . "::" . $gv->NAME;

    # When does a CV not fulfill ->ARRAY->ARRAY? Some time during
    # initialization?
    if (    $cv->can('PADLIST')
        and $cv->PADLIST->can('ARRAY')
        and $cv->PADLIST->ARRAY->can('ARRAY') )
    {
        push @anon_subs,
            map( (
                'CV' eq class($_)
                ? { root  => $_->ROOT,
                    start => $_->START
                    }
                : ()
            ),
            $cv->PADLIST->ARRAY->ARRAY );
    }

    return unless ( ( my $start = $cv->START )
        and ( my $root = $cv->ROOT ) );

    $starts{$name} = $start;
    $roots{$name}  = $root;

    #    return TRUE;
    return;
}

# sub B::SPECIAL::_B_Utils_init_sub_cache {
#
#     # This is a callback function called from B::Utils::_init via
#     # B::walksymtable.
#
#     # JJ: I'm not sure why this callback function exists.
#
#     return TRUE;
# }

=pod

=item C<$op->first>

=item C<$oo->last>

=item C<$op->other>

Normally if you call first, last or other on anything which is not an
UNOP, BINOP or LOGOP respectivly it will die.  This leads to lots of
code like:

    $op->first if $op->can('first');

B::Utils provides every op with first, last and other methods which
will simply return nothing if it isn't relevent.

=cut

# BEGIN {
#     for ( qw( first last other ) ) {
# 	eval "#line ".__LINE__." '".__FILE__.qq['\n
# sub B::OP::$_ {
#     my \$op = \$_[0];
#     my \$cv;
#     return( ( \$cv = \$op->can( "SUPER::first" ) )
#  	    ? \$op->\$cv
#  	    : () );
# }
#    ];
#         die $@ if $@;
#     }
# }

=pod

=item C<$op->oldname>

Returns the name of the op, even if it is currently optimized to null.
This helps you understand the stucture of the op tree.

=cut

sub B::OP::oldname {
    my $op   = $_[0];
    my $name = $op->name;
    my $targ = $op->targ;

    # This is a an operation which *used* to be a real op but was
    # optimized away. Fetch the old value and ignore the leading pp_.

    # I forget why the original pp # is located in the targ field.
    return $name eq 'null' && $targ
        ? substr( ppname($targ), 3 )
        : $name;

}

=pod

=item C<$op->kids>

Returns an array of all this op's non-null children, in order.

=cut

sub B::OP::kids {
    my $op = $_[0];
    return unless defined wantarray;

    if ( class($op) eq "LISTOP" ) {
        my @kids = $op->first;
        push @kids, $kids[-1]->sibling while $kids[-1]->can('sibling');
        pop @kids if 'NULL' eq class( $kids[-1] );

        @kids == $op->children or die;

        return @kids;
    }
    else {
        my @kids;

        push @kids, $op->first if $op->can('first');
        push @kids, $op->last  if $op->can('last');
        push @kids, $op->other if $op->can('other');
        return @kids;
    }
}

=pod

=item C<$op->parent>

Returns the parent node in the op tree, if possible. Currently
"possible" means "if the tree has already been optimized"; that is, if
we're during a C<CHECK> block. (and hence, if we have valid C<next>
pointers.)

In the future, it may be possible to search for the parent before we
have the C<next> pointers in place, but it'll take me a while to
figure out how to do that.

=cut

sub B::OP::parent {

    my $target  = shift;
    my $deadend = shift || { $$target => 1 };

    ### Parent of: $target->stringify

    if ( 'NULL' eq class( $target->next ) ) {
        ### Null next
        return;
    }

    # Simon Cozens' wrote this: "I'm not sure how to do this yet. I'm
    # sure there is a way. If you know, please email me."

    my ( $search_nodes, @todo, @done );
    $search_nodes = sub {
        my $node = $_[0]
            or return FALSE;
        'NULL' ne class($node)
            or return FALSE;

        # Go up a level if we've got stuck, and search (for the same
        # $target) from a higher vantage point.
        if ( exists $deadend->{$$node} ) {
            ### DEADEND: $node->stringify
            return;
        }
        else {
            ### Trying: $node->stringify
            push @done, $node;
        }

 #        if ( exists $deadend->{$$node} ) {
 #            if ( my $parent = $search_nodes->( $node->parent($deadend) ) ) {
 #                return $parent;
 #            }
 #        }

        unshift @todo, $node->kids;

        # Test the immediate children, but only children we haven't visited
        # already.
        my @new_kids = grep !$deadend->{$$_}, $node->kids;
        for (@new_kids) {
            if ( $$_ == $$target ) {
                return $node;
            }
        }

        # Recurse and examine each child, in turn.
        @todo = _uniq_ops( @new_kids, @todo );

        # Not in this subtree.
        ++$deadend->{$$node};
        return;
    };

    # Skip to the farthest sibling and make a list of each with the most
    # recent at the beginning of the list.

    # I am planning ahead for the day when it turns out that the parent
    # cannot be found in the last sibling somewhere. Maybe it is just a
    # null? I would like to be able to back track up the tree to find a
    # ->next node that will bring us to northeast of (or even better,
    # directly to) the parent.

    @todo = reverse( $target->siblings ), $_;
    while (1) {

        @todo = _uniq_ops(
            grep { 'NULL' ne class($_) and !$deadend->{$$_} }
                map {
                (   ( $_->can('last')  ? $_->last  : () ),
                    ( $_->can('other') ? $_->other : () ),
                    ( $_->can('next')  ? $_->next  : () ),
                    $_->kids,
                    $_->siblings,
                    $_
                    )
                } ( @todo, @done, $_ )
        );

        last if not @todo;

        while (@todo) {
            my $to_search = shift @todo;
            if ( my $parent = $search_nodes->($to_search) ) {
                weaken $search_nodes;
                return $parent;
            }
        }
    }

    # Having reached here... I give up?
    weaken $search_nodes;
    return;
}

sub _uniq_ops { my %seen; grep !$seen{$$_}++, @_ }

=pod

=item ->ancestors()

Undocumented.

=cut

sub ancestors {
    my @nodes;
    my $node = shift;
    while ( my $parent = $node->parent ) {
        push @nodes, $parent;
    }
    return @nodes;
}

=pod

=item ->descendants()

Undocumented.

=cut

sub descendants {
    my @nodes;
    my $node = shift;
    walkoptree_simple( $node, sub { push @nodes, $_ } );
    return @nodes;
}

=pod

=item ->siblings()

Undocumented.

=cut

sub B::OP::siblings {
    my @siblings = $_[0];
    push @siblings, $siblings[-1]->sibling
        until 'NULL' eq class $siblings[-1];
    shift @siblings;
    pop @siblings if 'NULL' eq class $siblings[-1];

    return @siblings;
}

=item C< $op->previous >

Like C< $op->next >, but not quite.

=cut

## sub B::OP::previous {
##     return unless defined wantarray;
##
##     my $target = $_[0];
##
##     my $start = $target;
##     my (%deadend, $search);
##     $search = sub {
##         my $node = $_[0];
##
##         unless ( defined $node ) {
##             # If I've been asked to search nothing, just return. The
##             # ->parent call might do this to me.
##             return FALSE;
##         }
##         elsif ( exists $deadend{$node} ) {
##             # If this node has been seen already, try again as its
##             # parent.
##             return $search->( $node->parent );
##         }
##         elsif ( eval { ${$node->next} == $$target } ) {
##             return $node;
##         }
##
##         # When searching the children, do it in reverse order because
##         # pointers back up are more likely to be farther down the
##         # stack. This works without reversing but I can avoid some
##         # work by ordering the work this way.
##         my @kids = reverse $node->kids;
##
##         # Search this node's direct children for the ->next pointer
##         # that points to this node.
##         eval { ${$_->can('next')} == $$target } and return $_->next
##           for @kids;
##
##         # For each child, check it for a match.
## 	my $found;
##         $found = $search->($_) and return $found
##           for @kids;
##
##         # Not in this subtree.
##         $deadend{$node} = TRUE;
##         return FALSE;
##     };
##
##     my $next = $target;
##     while ( eval { $next = $next->next } ) {
## 	my $result;
##         $result = $search->( $next )
##           and return $result;
##     }
##
##     return FALSE;
## }

sub B::OP::stringify {
    my $op = shift;

    return sprintf "%s %s=(0x%07x)", $op->name, class($op), $$op;
}

=pod

=item walkoptree_simple($op, \&callback, [$data]

The C<B> module provides various functions to walk the op tree, but
they're all rather difficult to use, requiring you to inject methods
into the C<B::OP> class. This is a very simple op tree walker with
more expected semantics.

All the C<walk> functions set C<B::Utils::file> and C<B::Utils::line>
to the appropriate values of file and line number in the program being
examined.

=cut

$File = '__none__';
$Line = 0;
$Sub  = undef;

sub walkoptree_simple {
    $File = '__none__';
    $Line = 0;

    &_walkoptree_simple;

    return TRUE;
}

sub _walkoptree_simple {
    my ( $op, $callback, $data ) = @_;

    if ( ref $op and $op->isa("B::COP") ) {
        $File = $op->file;
        $Line = $op->line;
    }

    $callback->( $op, $data );
    if (    ref $op
        and $$op
        and $op->flags & OPf_KIDS )
    {
        _walkoptree_simple( $_, $callback, $data ) for $op->kids;
    }

    return;

}

=pod

=item walkoptree_filtered($op, \&filter, \&callback, [$data])

This is much the same as C<walkoptree_simple>, but will only call the
callback if the C<filter> returns true. The C<filter> is passed the
op in question as a parameter; the C<opgrep> function is fantastic
for building your own filters.

=cut

sub walkoptree_filtered {
    $File = '__none__';
    $Line = 0;

    &_walkoptree_filtered;

    return TRUE;
}

sub _walkoptree_filtered {
    my ( $op, $filter, $callback, $data ) = @_;

    if ( $op->isa("B::COP") ) {
        $File = $op->file;
        $Line = $op->line;
    }

    $callback->( $op, $data ) if $filter->($op);

    if (    ref $op
        and $$op
        and $op->flags & OPf_KIDS )
    {

        my $kid = $op->first;
        while ( ref $kid
            and $$kid )
        {
            _walkoptree_filtered( $kid, $filter, $callback, $data );

            $kid = $kid->sibling;
        }
    }

    return TRUE;
}

=pod

=item walkallops_simple(\&callback, [$data])

This combines C<walkoptree_simple> with C<all_roots> and C<anon_subs>
to examine every op in the program. C<$B::Utils::sub> is set to the
subroutine name if you're in a subroutine, C<__MAIN__> if you're in
the main program and C<__ANON__> if you're in an anonymous subroutine.

=cut

sub walkallops_simple {
    $Sub = undef;

    &_walkallops_simple;

    return TRUE;
}

sub _walkallops_simple {
    my ( $callback, $data ) = @_;

    _init();

    walkoptree_simple( $_, $callback, $data ) for values %roots;

    $Sub = "__ANON__";
    walkoptree_simple( $_->{root}, $callback, $data ) for @anon_subs;

    return TRUE;
}

=pod

=item walkallops_filtered(\&filter, \&callback, [$data])

Same as above, but filtered.

=cut

sub walkallops_filtered {
    $Sub = undef;

    &_walkallops_filterd;

    return TRUE;
}

sub _walkallops_filtered {
    my ( $filter, $callback, $data ) = @_;

    _init();

    walkoptree_filtered( $_, $filter, $callback, $data ) for values %roots;

    $Sub = "__ANON__";

    walkoptree_filtered( $_->{root}, $filter, $callback, $data )
        for @anon_subs;

    return TRUE;
}

=pod

=item opgrep(\%conditions, @ops)

Returns the ops which meet the given conditions. The conditions should
be specified like this:

    @barewords = opgrep(
                        { name => "const", private => OPpCONST_BARE },
                        @ops
                       );

You can specify alternation by giving an arrayref of values:

    @svs = opgrep ( { name => ["padsv", "gvsv"] }, @ops)

And you can specify inversion by making the first element of the
arrayref a "!". (Hint: if you want to say "anything", say "not
nothing": C<["!"]>)

You may also specify the conditions to be matched in nearby ops.

    walkallops_filtered(
        sub { opgrep( {name => "exec",
                       next => {
                                 name    => "nextstate",
                                 sibling => { name => [qw(! exit warn die)] }
                               }
                      }, @_)},
        sub {
              carp("Statement unlikely to be reached");
              carp("\t(Maybe you meant system() when you said exec()?)\n");
        }
    )

Get that?

Here are the things that can be tested:

        name targ type seq flags private pmflags pmpermflags
        first other last sibling next pmreplroot pmreplstart pmnext

=item opgrep( \@conditions, @ops )

Same as sbove, except that you don't have to chain the conditions
yourself.  If you pass an array-ref, opgrep will chain the conditions
for you.  The conditions can either be strings (taken as op-names), or
hash-refs, with the same testable conditions as given above.

=cut

sub opgrep {
    return unless defined wantarray;

    my $conds_ref = $_[0];
    $conds_ref = _opgrep_helper($conds_ref)
        if 'ARRAY' eq ref $conds_ref;

    my @grep_ops;

OP:
    for my $op (@_) {
        next unless ref $op and $$op;

        # First, let's skim off ops of the wrong type. If they require
        # something that isn't implemented for this kind of object, it
        # must be wrong. These tests are cheap
        exists $conds_ref->{$_}
            and !$op->can($_)
            and next
            for
            qw( first other last pmreplroot pmreplstart pmnext pmflags pmpermflags name targ type seq flags private  );

        (   ref( $conds_ref->{$_} )
            ? ( "!" eq $conds_ref->{$_}[0]
                ? ()
                : ()
                )
            : ( $op->$_ eq $conds_ref->{$_} or next )
            )
            for qw( name targ type seq flags private pmflags pmpermflags );

        for my $test (
            qw(name targ type seq flags private pmflags pmpermflags))
        {
            next unless exists $conds_ref->{$_};
            my $val = $op->$test;

            if ( 'ARRAY' eq ref $conds_ref->{$test} ) {

                # Test a list of valid/invalid values.
                if ( '!' eq $conds_ref->{$test}[0] ) {

                    # Fail if any entries match.
                    $_ eq $val
                        or next OP
                        for @{ $conds_ref->{$test} }
                        [ 1 .. $#{ $conds_ref->{$test} } ];
                }
                else {

                    # Fail if any entries don't match.
                    $_ ne $val
                        or next OP
                        for @{ $conds_ref->{$test} };
                }
            }
            elsif ( 'CODE' eq ref $conds_ref->{$test} ) {
                local $_ = $val;
                $conds_ref->{$test}($op)
                    or next OP;
            }
            else {

                # Test a single value.
                $conds_ref->{$test} eq $op->$test
                    or next OP;
            }
        }

        # We know it ->can because that was tested above. It is an
        # error to have anything in this list of tests that isn't
        # tested for ->can above.
        exists $conds_ref->{$_}
            and not( opgrep( $conds_ref->{$_}, $op->$_ ) )
            and next
            for
            qw( first other last sibling next pmreplroot pmreplstart pmnext );

        # Attempt to quit early if possible.
        if (wantarray) {
            push @grep_ops, $_;
        }
        elsif ( defined wantarray ) {
            return TRUE;
        }
    }

    # Either this was called in list context and then I want to just
    # return everything possible or this is in scalar/void context and
    # @grep_ops will be empty and thus "false."
    return @grep_ops;
}

sub _opgrep_helper {
    my @conds =
        map ref() ? {%$_} : { name => $_ }, @{ $_[0] };

    # Wire this into a list of entries, all ->next
    for ( 1 .. $#conds ) {
        $conds[ $_ - 1 ]{next} = $conds[$_];
    }

    # This is a linked list now so I can return only the head.
    return $conds[0];
}

=pod

=item carp(@args)

=item croak(@args)

Warn and die, respectively, from the perspective of the position of
the op in the program. Sounds complicated, but it's exactly the kind
of error reporting you expect when you're grovelling through an op
tree.

=cut

sub carp (@)  { CORE::warn( _preparewarn(@_) ) }
sub croak (@) { CORE::die( _preparewarn(@_) ) }

sub _preparewarn {
    my $args = join '', @_;
    $args = "Something's wrong " unless $args;
    if ( "\n" ne substr $args, -1, 1 ) {
        $args .= " at $File line $Line.\n";
    }
    return $args;
}

=back

=head2 EXPORT

None by default.

=head1 AUTHOR

Originally written by Simon Cozens, C<simon@cpan.org>
Maintained by Joshua ben Jore, C<jjore@cpan.org>

Contributions from Mattia Barbon and Jim Cromie.

=head1 TODO

I need to add more Fun Things, and possibly clean up some parts where
the (previous/parent) algorithm has catastrophic cases, but it's more
important to get this out right now than get it right.

=head1 SEE ALSO

L<B>, L<B::Generate>.

=cut

1;
