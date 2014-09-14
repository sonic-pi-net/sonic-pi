! Copyright (C) 2008 Slava Pestov
! See http://factorcode.org/license.txt for BSD license.
USING: accessors kernel hashtables calendar random assocs
namespaces make splitting sequences sorting math.order present
io.files io.directories io.encodings.ascii
syndication farkup
html.components html.forms
http.server
http.server.dispatchers
furnace.actions
furnace.utilities
furnace.redirection
furnace.auth
furnace.auth.login
furnace.boilerplate
furnace.syndication
validators
db.types db.tuples lcs urls ;
IN: webapps.wiki

: wiki-url ( rest path -- url )
    [ "$wiki/" % % "/" % present % ] "" make
    <url> swap >>path ;

: view-url ( title -- url ) "view" wiki-url ;

: edit-url ( title -- url ) "edit" wiki-url ;

: revisions-url ( title -- url ) "revisions" wiki-url ;

: revision-url ( id -- url ) "revision" wiki-url ;

: user-edits-url ( author -- url ) "user-edits" wiki-url ;

TUPLE: wiki < dispatcher ;

SYMBOL: can-delete-wiki-articles?

can-delete-wiki-articles? define-capability

TUPLE: article title revision ;

article "ARTICLES" {
    { "title" "TITLE" { VARCHAR 256 } +not-null+ +user-assigned-id+ }
    { "revision" "REVISION" INTEGER +not-null+ } ! revision id
} define-persistent

: <article> ( title -- article ) article new swap >>title ;

TUPLE: revision id title author date content description ;

revision "REVISIONS" {
    { "id" "ID" INTEGER +db-assigned-id+ }
    { "title" "TITLE" { VARCHAR 256 } +not-null+ } ! article id
    { "author" "AUTHOR" { VARCHAR 256 } +not-null+ } ! uid
    { "date" "DATE" TIMESTAMP +not-null+ }
    { "content" "CONTENT" TEXT +not-null+ }
    { "description" "DESCRIPTION" TEXT }
} define-persistent

M: revision feed-entry-title
    [ title>> ] [ drop " by " ] [ author>> ] tri 3append ;

M: revision feed-entry-date date>> ;

M: revision feed-entry-url id>> revision-url ;

: reverse-chronological-order ( seq -- sorted )
    [ date>> ] inv-sort-with ;

: <revision> ( id -- revision )
    revision new swap >>id ;

: validate-title ( -- )
    { { "title" [ v-one-line ] } } validate-params ;

: validate-author ( -- )
    { { "author" [ v-username ] } } validate-params ;

: <article-boilerplate> ( responder -- responder' )
    <boilerplate>
        { wiki "page-common" } >>template ;

: <main-article-action> ( -- action )
    <action>
        [ "Front Page" view-url <redirect> ] >>display ;

: latest-revision ( title -- revision/f )
    <article> select-tuple
    dup [ revision>> <revision> select-tuple ] when ;

: <view-article-action> ( -- action )
    <action>

        "title" >>rest

        [ validate-title ] >>init

        [
            "title" value dup latest-revision [
                from-object
                { wiki "view" } <chloe-content>
            ] [
                edit-url <redirect>
            ] ?if
        ] >>display

    <article-boilerplate> ;

: <view-revision-action> ( -- action )
    <page-action>

        "id" >>rest

        [
            validate-integer-id
            "id" value <revision>
            select-tuple from-object
        ] >>init

        { wiki "view" } >>template
    
    <article-boilerplate> ;

: <random-article-action> ( -- action )
    <action>
        [
            article new select-tuples random
            [ title>> ] [ "Front Page" ] if*
            view-url <redirect>
        ] >>display ;

: amend-article ( revision article -- )
    swap id>> >>revision update-tuple ;

: add-article ( revision -- )
    [ title>> ] [ id>> ] bi article boa insert-tuple ;

: add-revision ( revision -- )
    [ insert-tuple ]
    [
        dup title>> <article> select-tuple
        [ amend-article ] [ add-article ] if*
    ]
    bi ;

: <edit-article-action> ( -- action )
    <page-action>

        "title" >>rest

        [
            validate-title

            "title" value <article> select-tuple
            [ revision>> <revision> select-tuple ]
            [ f <revision> "title" value >>title ]
            if*

            [ title>> "title" set-value ]
            [ content>> "content" set-value ]
            bi
        ] >>init

        { wiki "edit" } >>template

    <article-boilerplate> ;

: <submit-article-action> ( -- action )
    <action>
        [
            validate-title

            {
                { "content" [ v-required ] }
                { "description" [ [ v-one-line ] v-optional ] }
            } validate-params

            f <revision>
                "title" value >>title
                now >>date
                username >>author
                "content" value >>content
                "description" value >>description
            [ add-revision ] [ title>> view-url <redirect> ] bi
        ] >>submit

    <protected>
        "edit wiki articles" >>description ;

: <revisions-boilerplate> ( responder -- responder )
    <boilerplate>
        { wiki "revisions-common" } >>template ;

: list-revisions ( -- seq )
    f <revision> "title" value >>title select-tuples
    reverse-chronological-order ;

: <list-revisions-action> ( -- action )
    <page-action>

        "title" >>rest

        [
            validate-title
            list-revisions "revisions" set-value
        ] >>init

        { wiki "revisions" } >>template

    <revisions-boilerplate>
    <article-boilerplate> ;

: <list-revisions-feed-action> ( -- action )
    <feed-action>

        "title" >>rest

        [ validate-title ] >>init

        [ "Revisions of " "title" value append ] >>title

        [ "title" value revisions-url ] >>url

        [ list-revisions ] >>entries ;

: rollback-description ( description -- description' )
    [ "Rollback of '" "'" surround ] [ "Rollback" ] if* ;

: <rollback-action> ( -- action )
    <action>

        [ validate-integer-id ] >>validate

        [
            "id" value <revision> select-tuple
                f >>id
                now >>date
                username >>author
                [ rollback-description ] change-description
            [ add-revision ]
            [ title>> revisions-url <redirect> ] bi
        ] >>submit
    
    <protected>
        "rollback wiki articles" >>description ;

: list-changes ( -- seq )
    f <revision> select-tuples
    reverse-chronological-order ;

: <list-changes-action> ( -- action )
    <page-action>
        [ list-changes "revisions" set-value ] >>init
        { wiki "changes" } >>template

    <revisions-boilerplate> ;

: <list-changes-feed-action> ( -- action )
    <feed-action>
        [ URL" $wiki/changes" ] >>url
        [ "All changes" ] >>title
        [ list-changes ] >>entries ;

: <delete-action> ( -- action )
    <action>

        [ validate-title ] >>validate

        [
            "title" value <article> delete-tuples
            f <revision> "title" value >>title delete-tuples
            URL" $wiki" <redirect>
        ] >>submit

     <protected>
        "delete wiki articles" >>description
        { can-delete-wiki-articles? } >>capabilities ;

: <diff-action> ( -- action )
    <page-action>

        [
            {
                { "old-id" [ v-integer ] }
                { "new-id" [ v-integer ] }
            } validate-params

            "old-id" "new-id"
            [ value <revision> select-tuple ] bi@
            [
                over title>> "title" set-value
                [ "old" [ from-object ] nest-form ]
                [ "new" [ from-object ] nest-form ]
                bi*
            ]
            [ [ content>> string-lines ] bi@ diff "diff" set-value ]
            2bi
        ] >>init

        { wiki "diff" } >>template

    <article-boilerplate> ;

: <list-articles-action> ( -- action )
    <page-action>

        [
            f <article> select-tuples
            [ title>> ] sort-with
            "articles" set-value
        ] >>init

        { wiki "articles" } >>template ;

: list-user-edits ( -- seq )
    f <revision> "author" value >>author select-tuples
    reverse-chronological-order ;

: <user-edits-action> ( -- action )
    <page-action>

        "author" >>rest

        [
            validate-author
            list-user-edits "revisions" set-value
        ] >>init

        { wiki "user-edits" } >>template

    <revisions-boilerplate> ;

: <user-edits-feed-action> ( -- action )
    <feed-action>
        "author" >>rest
        [ validate-author ] >>init
        [ "Edits by " "author" value append ] >>title
        [ "author" value user-edits-url ] >>url
        [ list-user-edits ] >>entries ;

: init-sidebars ( -- )
    "Contents" latest-revision [ "contents" [ from-object ] nest-form ] when*
    "Footer" latest-revision [ "footer" [ from-object ] nest-form ] when* ;

: init-relative-link-prefix ( -- )
    URL" $wiki/view/" adjust-url present relative-link-prefix set ;

: <wiki> ( -- dispatcher )
    wiki new-dispatcher
        <main-article-action> "" add-responder
        <view-article-action> "view" add-responder
        <view-revision-action> "revision" add-responder
        <random-article-action> "random" add-responder
        <list-revisions-action> "revisions" add-responder
        <list-revisions-feed-action> "revisions.atom" add-responder
        <diff-action> "diff" add-responder
        <edit-article-action> "edit" add-responder
        <submit-article-action> "submit" add-responder
        <rollback-action> "rollback" add-responder
        <user-edits-action> "user-edits" add-responder
        <list-articles-action> "articles" add-responder
        <list-changes-action> "changes" add-responder
        <user-edits-feed-action> "user-edits.atom" add-responder
        <list-changes-feed-action> "changes.atom" add-responder
        <delete-action> "delete" add-responder
    <boilerplate>
        [ init-sidebars init-relative-link-prefix ] >>init
        { wiki "wiki-common" } >>template ;

: init-wiki ( -- )
    "resource:extra/webapps/wiki/initial-content" [
        [
            dup ".txt" ?tail [
                swap ascii file-contents
                f <revision>
                    swap >>content
                    swap >>title
                    "slava" >>author
                    now >>date
                add-revision
            ] [ 2drop ] if
        ] each
    ] with-directory-files ;
