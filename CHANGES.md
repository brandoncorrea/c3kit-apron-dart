### 2.3.0 (Sept 21, 2025)
* adds bad-words/contains-profanity? function

### 2.2.3
* Fixes issue where `count-by` throws when no parameters are provided.

### 2.2.2
* Fixes case where test-fabric fails to query seqable attributes by pattern matching

### 2.2.1
* Fixes c3kit.apron.log `time` override warning

### 2.2.0 
 * adds log/time (Feb 10, 2025)
 * pickle namespace (Apr 22, 2025)

### 2.1.5 (Nov 18, 2024)
 * schema conform can handle nested coercion

### 2.1.4 (May 28, 2024)
 * Upgrades deps
 * Passes all specs under advanced cljs optimizations
 * Fixes discrepancy with `c3kit.apron.corec/oset-in` between `:none` and `:advanced` optimizations where it would throw in `:none`, but not `:advanced`.

### 2.1.3 (Mar 18, 2024)
 * c3kit.apron.schema changed
   * c3kit.apron.schema is now self defining. Schema provides a schema for schema definitions.
   * new :seq type allows more clear specification of sequential values, but also allows the processing of the seq itself
   * new :map type clarifies the use of nested schemas
   * new :one-of type allows a field to take any number of shapes
   * adds normalize-schema and normalize-spec to expand shorthands (backwards compatibility)
   * adds conform-schema to normalize, coerce, and validate schemas
   * legend build will conform-schema! on all schema
   * rename `<process>-errors` to `<process>-message-map`
   * backwards compatible fns have been DEPRECATED 

### 2.1.0 (Feb 3, 2024)
 * c3kit.apron.schema overhaul - minor breaking changes
   * For 98% of the usage, there should be no visible changes, however there are some breaking changes.
   * Failed operation no longer return the unintelligible SchemaError. Instead, it returns a map where the problematic keys have FieldErrors as values.  This should be much more printable and readable.
   * Fixes bug where coerce, validate and conform returned different results
   * Error messages should be more detailed where possible

### July 14, 2023
 * c3kit.apron.app/start-env app must be the first parameter
