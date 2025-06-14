# Apron

![Apron](https://github.com/cleancoders/c3kit/blob/master/img/apron_200.png?raw=true)

A library component of [c3kit - Clean Coders Clojure Kit](https://github.com/cleancoders/c3kit).

_"Where is thy leather apron and thy rule?"_ - Shakespeare

[![Apron Build](https://github.com/cleancoders/c3kit-apron/actions/workflows/test.yml/badge.svg)](https://github.com/cleancoders/c3kit-apron/actions/workflows/test.yml)

Apron consists of necessities that almost any clojure app would find useful.

 * __app.clj__ : application service and state management
 * __util.clj__ : misc utilities used by other c3kit code
 * __corec.cljc__ : useful fns, platform independent
 * __cursor.cljc__ : atom cursor based on reagent's
 * __legend.cljc__ : index application entities
 * __log.cljc__ : platform independent logging
 * __schema.cljc__ : validation, coercion, specification for entity structure
 * __time.cljc__ : simple platform independent time manipulation
 * __utilc.cljc__ : platform independent edn, transit, csv, etc..

# Development

    # Run the JVM tests
    clj -X:test:clj

    # Compile and Run JS tests
    clj -M:test:cljs

    # Run the Dart tests
    clj -M:test:cljd test 
