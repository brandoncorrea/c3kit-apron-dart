# Schema

### c3kit.apron.schema

## Overview

The `schema` library aims to solve the following problems:

* What is the expected shape of the data?
* How can I `coerce` data into the desired shape?
* How can I `validate` data?
* How can I `present` data?

## Example

Consider this map that represents a point.

``` clojure
{:kind :point
 :x    1
 :y    2}
```

The data structure includes a `:kind` to tell us what the data represents.  Isn't that nice?  And it contains the necessary data, `:x` and `:y`, to represent a point.

We can define the shape of this data structure, its schema, like so:  

``` clojure
(def point {:kind {:type :keyword}
            :x    {:type :int}
            :y    {:type :int}})
```

Yes, it's just data; a map where each key is mapped to a `spec` (specification).  This is the `schema` schema; the shape of data used to define the shape of other data.  *(The terminology in this document is circular and self-referencing. Bare with me.)*

Let's start using this schema.

## Coercion

Assume we were given the data below, with the assertion that the data represents a `point`.

``` clojure
(def data {:kind :point
           :x    "1"
           :y    "2"})
```

Notice that the `:x` and `:y` values are strings.  That's not right.  They're supposed to be integers. No worries. Using our `point` schema, we can `coerce` the data in the shape we want.

``` clojure
(require '[c3kit.apron.schema :as schema])
(schema/coerce point data)
=> {:kind :point, :x 1, :y 2}         
```

Now, imagine we were given this strange `point` data.

``` clojure
(def data {:kind :point
           :x    ["1"]
           :y    ["2"]})
```

When we try to `coerce` this, we get the following.

``` clojure
(schema/coerce point data)
=>
{:kind :point,
 :x #c3kit.apron.schema.CoerceError{:message "can't coerce [\"1\"] to int"},
 :y #c3kit.apron.schema.CoerceError{:message "can't coerce [\"2\"] to int"}}
```

Notice how the `:x` and `:y` keys map to `CoerceError` objects containing friendly messages.  We'll use those later.

With some enhancements to our `point` schema, we can handle this type of `point` data.

``` clojure
(def point {:kind {:type :keyword}
            :x    {:type :int :coerce (fn [v] (schema/->int (first v)))}
            :y    {:type :int :coerce first}})
            
(schema/coerce point data)
=> {:kind :point, :x 1, :y 2}
```

For `:x` we add `:coerce` to the `spec` mapping to a **coerce function**, and we are very explicit about how to handle the data.  But for `:y` we take a shortcut.  The value for `:coerce` must be a **coerce function** or a list of **coerce function**s.  Each **coerce function** must take the value as a parameter, and return the coerced value.  So by using the function `first` for `:y` we end up with the string `"2"`... yet our result has the integer `2`. The final step of the `coerce` operation is **type-coercion**, using the `:type` of the spec.  `schema` knows how to convert a string to an int, so it takes care of that for you.

## Validation

Given these bits of data:   

``` clojure
(def data1 {:kind :point
            :x    1
            :y    2})
(def data2 {:kind :point
            :x    "1"
            :y    "2"})
```

Let's use `schema` to validate them.

``` clojure
(schema/validate point data1)
=> {:kind :point, :x 1, :y 2}

(schema/validate point data2)
=>
{:kind :point,
 :x #c3kit.apron.schema.ValidateError{:message "is invalid"},
 :y #c3kit.apron.schema.ValidateError{:message "is invalid"}}
```

`data1` looks good but `data2` has problems.  `:x` and `:y` are strings, not ints!  `schema` is performing **type-validation** on the values based on the `:type` of the spec.

Again you can see the problematic fields are mapped to errors, `ValidationError` objects this time.  However, the error message `"is invalid"` is not terribly useful.  Let's improve that.

``` clojure
(def point {:kind {:type :keyword}
            :x    {:type :int :message "must be an int"}
            :y    {:type :int :message "must be an int"}})
            
(schema/validate point data2)
=>
{:kind :point,
 :x #c3kit.apron.schema.ValidateError{:message "must be an int"},
 :y #c3kit.apron.schema.ValidateError{:message "must be an int"}}
```

We added a `:message` string to the spec. Now the `:message` value is being used for `coerce` and `validate` error messages.

Let's be more restrictive about our points.  For the fun of it, `:x` must be even and `:y` must be odd. 

``` clojure
(def point {:kind {:type :keyword}
            :x    {:type     :int
                   :message  "must be an even int"
                   :validate even?}
            :y    {:type     :int
                   :message  "must be an odd int"
                   :validate odd?}})
                   
(schema/validate point {:kind :point :x "2" :y "1"})
=>
{:kind :point,
 :x #c3kit.apron.schema.ValidateError{:message "must be an even int"},
 :y #c3kit.apron.schema.ValidateError{:message "must be an odd int"}}   
                 
(schema/validate point {:kind :point :x 1 :y 2})
=>
{:kind :point,
 :x #c3kit.apron.schema.ValidateError{:message "must be an even int"},
 :y #c3kit.apron.schema.ValidateError{:message "must be an odd int"}}
```

We added new validations using the `:validate` entry in the spec that maps to a **validate function**.  `:validate` can also map to a list of **validate function**s that will be performed in order.  Each **validate function** must take the value as a parameter and return a truthy value if the input is valid, and falsy otherwise.   

This example demonstrates that there are two validations being performed on both `:x` and `:y`; the `even?`/`odd?` validations specified by the `:validate` entry in the spec, and the automatic **type-validation**.  The **type-validation** is the first step in the `validate` operation, whereas **type-coercion** is that last step of the `coerce` operation.  This demonstrated in the example below.  

Also, let's make sure we get a descriptive error messages for each validation?

```clojure
(def point {:kind {:type :keyword}
            :x    {:type        :int
                   :message     "must be an int"
                   :validations [{:validate even? :message "must be even"}
                                 {:validate #(<= 0 % 100) :message "out of range"}]}
            :y    {:type        :int
                   :message     "must be an int"
                   :validations [{:validate odd? :message "must be odd"}
                                 {:validate #(<= 0 % 100) :message "out of range"}]}})

(schema/validate point {:kind :point :x "101" :y "102"})
=>
{:kind :point,
 :x #c3kit.apron.schema.ValidateError{:message "must be an int"},
 :y #c3kit.apron.schema.ValidateError{:message "must be an int"}}

(schema/validate point {:kind :point :x 1 :y 2})
=>
{:kind :point,
 :x #c3kit.apron.schema.ValidateError{:message "must be even"},
 :y #c3kit.apron.schema.ValidateError{:message "must be odd"}}

(schema/validate point {:kind :point :x 102 :y 101})
=>
{:kind :point,
 :x #c3kit.apron.schema.ValidateError{:message "out of range"},
 :y #c3kit.apron.schema.ValidateError{:message "out of range"}}
```

The `:validations` entry in the spec allows us to have any number of validations, each with their own message.  Each validation is a map that must have `:validate` that maps to a **validate function**, and an optional `:message`.  If `:message` is not provided in the validation map, `schema` will use the `:message` value in the `spec` if it exists, or `"is invalid"`.

`{:kind :point :x "101" :y "102"}` is invalid in all the ways we check.  But the message we get is `"must be an int"` which is our root `:message`.  This demonstrates that the **type-validation** has to pass first before any other validations take place.

## Conform

Often, we need to `coerce` data and then `validate` the coerced data.  'schema' offers a `conform` operation to take care of this for you. 

```clojure
(schema/conform point {:kind :point :x "2" :y "1"})
=> {:kind :point, :x 2, :y 1}

(schema/conform point {:kind :point :x "blah" :y "2"})
=>
{:kind :point,
 :x #c3kit.apron.schema.CoerceError{:message "must be an int"},
 :y #c3kit.apron.schema.ValidateError{:message "must be odd"}}
```

That first `point` is perfect and the result of `conform` is a map shaped just how we like it.  The second point has a couple problems and the result of `conform` identifies them nicely. `:x` cannot be coerced and `:y` is invalid.

## Present

`schema` provides a `present` operation to make our data presentable to a user, or an API, or whatever.   

``` clojure
(schema/present point {:kind :point :x 1 :y 2})
=> {:kind :point, :x 1, :y 2}
```

Yeah, that looks the same.  We need to tell `schema` how to make the data presentable.

``` clojure
(def point {:kind {:type :keyword}
            :x    {:type :int :present #(str "X=" %)}
            :y    {:type :int :present #(str "Y=" %)}})
(schema/present point {:kind :point :x 1 :y 2})
=> {:kind :point, :x "X=1", :y "Y=2"}
```

The `:present` entry of the spec must map to a **present function** that takes the value as a parameter and returns a presentable value.  Unlike `:coerce` and `:validate`, `:present` may NOT map to a list of **present function**s.  

## Entity Level Specs

We have a new requirement for our geometry data.  A `point` may not be close to the origin (0, 0).  If a `point` has a distance from the origin of less than 5, it is invalid.  So far, we've only been able to validate a single field.  We need both `:x` and `:y` to calculate distance, and **entity level spec** give that ability.

``` clojure
(defn square [n] (* n n))
(defn distance [point] (Math/sqrt (+ (square (:x point)) (square (:y point)))))
(def point {:kind {:type :keyword}
            :x    {:type :int}
            :y    {:type :int}
            :*    {:distance {:coerce distance
                              :validate #(>= (distance %) 5) 
                              :message "too close to origin"}}})

(schema/coerce point {:kind :point :x 1 :y 2})
=> {:kind :point, :x 1, :y 2, :distance 2.23606797749979}

(schema/validate point {:kind :point :x 1 :y 2})
=> {:kind :point, :x 1, :y 2, :distance #c3kit.apron.schema.ValidateError{:message "too close to origin"}}

(schema/coerce point {:kind :point :x 4 :y 4})
=> {:kind :point, :x 4, :y 4, :distance 5.656854249492381}

(schema/validate point {:kind :point :x 4 :y 4})
=> {:kind :point, :x 4, :y 4}
```

`:*` is a special key in `schema` that maps to **entity level spec**s.  The **entity level spec**s can have all the same entries as normal field specs with one important difference: All the **coerce functions**, **validate functions**, and **present functions** take the whole entity as a parameter.  This allows them to access any or all of the fields in the entity to perform the desired operation.   

Also notice that we added a new field, `:distance`, in the **entity level spec**s.   That works.  **Entity level spec**s may also be applied to existing fields. 

## Nested Structures

### Maps

What would a `line` data structure look like?  A `line` consists of two `points`; a start and an end.

```clojure
(def point {:kind {:type :keyword}
            :x    {:type :int}
            :y    {:type :int}})

(def line {:kind  {:type :keyword}
           :start {:type :map :schema point}
           :end   {:type :map :schema point}})

(schema/conform line {:kind :line
                      :start {:kind :point :x "1" :y "2"}
                      :end {:kind :point :x 3.45 :y 6.78}})
=> {:kind :line, :start {:kind :point, :x 1, :y 2}, :end {:kind :point, :x 3, :y 6}}
```

Take a close look at the `:start` and `:end` of `line`.  The `:type` is set to `:map`.  `schema` will happily dive into nested data structures, however it needs to know what to expect.  So included in the spec is the `:schema`.  For a `line`, we want both `:start` and `:end` to be `points`.

### Seq

Now consider a `polygon` that may consist of many `points`.

``` clojure
(def polygon {:kind   {:type :keyword}
              :points {:type        :seq
                       :spec        {:type :map :schema point}
                       :validations [{:validate #(>= (count %) 4) :message "must have at least 4 points"}
                                     {:validate #(= (first %) (last %)) :message "not closed"}]}})

(schema/conform polygon {:kind :polygon})
=> {:kind :polygon, :points #c3kit.apron.schema.ValidateError{:message "must have at least 4 points"}}

(schema/conform polygon {:kind  :polygon
                         :points [{:kind :point :x "1" :y "2"}
                                  {:kind :point :x 3.45 :y 6.78}
                                  {:kind :point :x 6 :y 4}
                                  {:kind :point :x 99 :y 99}]})
=> {:kind :polygon, :points #c3kit.apron.schema.ValidateError{:message "not closed"}}

(schema/conform polygon {:kind  :polygon
                         :points [{:kind :point :x "1" :y "2"}
                                  {:kind :point :x 3.45 :y 6.78}
                                  {:kind :point :x 6 :y 4}
                                  {:kind :point :x 1 :y 2}]})
=>
{:kind :polygon,
 :points [{:kind :point, :x 1, :y 2} {:kind :point, :x 3, :y 6} {:kind :point, :x 6, :y 4} {:kind :point, :x 1, :y 2}]}
```

`polygon` has a `:points` field that is rather sophisticated.  We can see that the `:type` is `:seq`.  This tells `schema` that the `:points` value should be a sequential value (list, vector, set, ...), but `schema` needs to know what each value in the sequence should look like.  Therefor we also include the `:spec` in the spec.  In the case of the `polygon`, each value in `:points` should be a `point`.   

`schema` will expect seqs to hold a single type of value.  Said in another way, all the values in a seq must conform to the same spec.  If you need to process data where lists store values of different types, you can either use `:type :map` and specify a `:schema` where the keys are indexes of the list, or use `:type :ignore` and manually handle the operations. 

### One Of

We've got all these cool geometry data structures.  Wouldn't it be nice if we could have a `geometry` data structure that could hold on to one of the other geometry data structures?  Let's update our schemas to handle this.

``` clojure
(def point {:kind (schema/kind :point)
            :x    {:type :int}
            :y    {:type :int}})

(def line {:kind  (schema/kind :line)
           :start {:type :map :schema point}
           :end   {:type :map :schema point}})

(def circle {:kind   (schema/kind :circle)
             :center {:type :map :schema point}
             :radius {:type :int}})

(def geometry {:kind     (schema/kind :geometry)
               :geometry {:type :one-of :specs [{:type :map :schema point}
                                                {:type :map :schema line}
                                                {:type :map :schema circle}]}})
```

First, we set the `:kind` of each schema using `schema/kind`.  This helper function returns a `spec` that conforms and validates the value such that it must be the specified keyword.  Here's the implementation:

```clojure
(defn kind [key]
  {:type     :keyword
   :value    key
   :validate [#(or (nil? %) (= key %))]
   :coerce   [#(or % key)]
   :message  (str "mismatch; must be " key)})
```

Second, we added a `circle` schema.

And lastly, we have `geometry`. The `:type` of it's `:geometry` field is `:one-of`.  This allows the value to take on any "one of" the geometries specified by the `:specs` list.  `schema` will run the operation (`conform` in the case below) on the value against each `spec` in the `:specs` list until one succeeds.  If none of the operations succeed, the result will be an error.

```clojure
(schema/conform geometry {:kind :geometry :geometry {:kind :point :x "1" :y "2"}})
=> {:kind :geometry, :geometry {:kind :point, :x 1, :y 2}}

(schema/conform geometry {:kind :geometry :geometry {:kind  :line
                                                     :start {:kind :point :x "1" :y "2"}
                                                     :end   {:kind :point :x 3.45 :y 6.78}}})
=> {:kind :geometry, :geometry {:kind :line, :start {:kind :point, :x 1, :y 2}, :end {:kind :point, :x 3, :y 6}}}

(schema/conform geometry {:kind :geometry :geometry {:kind   :circle
                                                     :center {:kind :point :x "1" :y "2"}
                                                     :radius 42}})
=> {:kind :geometry, :geometry {:kind :circle, :center {:kind :point, :x 1, :y 2}, :radius 42}}

(schema/conform geometry {:kind :geometry :geometry {:kind :squiggle}})
=> {:kind :geometry, :geometry #c3kit.apron.schema.ConformError{:message "one-of: no matching spec"}}
```

## Dealing With Errors

We've seen that when errors occur during `schema` operations, they appear as the value the offending field in the result. `schema/error?` makes it easy to tell if a result has an error.

``` clojure
(schema/error? (schema/validate point {:kind :point :x 1 :y 2}))
=> false
(schema/error? (schema/validate point {:kind :point :x "blah" :y 2}))
=> true
```

Sometimes you want a list of all the errors. 

``` clojure
(schema/message-seq (schema/validate point {:kind :point :x 1 :y 2}))
=> nil

(schema/message-seq (schema/validate point {:kind :point :x "blah" :y 2}))
=> ["x is invalid"]

(schema/message-seq (schema/conform line {:kind  :line
                                          :start {:kind :point :x "blah" :y "2"}
                                          :end   {:kind :point :x 3.45 :y "blah"}}))
=> ["start.x can't coerce \"blah\" to int" "end.y can't coerce \"blah\" to int"]
```

Notice how the keys from nested errors are joined with a `.`, as in `start.x`.

More often you want a map of the errors.

``` clojure
(schema/message-map (schema/validate point {:kind :point :x 1 :y 2}))
=> nil

(schema/message-map (schema/validate point {:kind :point :x "blah" :y 2}))
=> {:x "is invalid"}

(schema/message-map (schema/conform line {:kind  :line
                                          :start {:kind :point :x "blah" :y "2"}
                                          :end   {:kind :point :x 3.45 :y "blah"}}))
=> {:start {:x "can't coerce \"blah\" to int"}, :end {:y "can't coerce \"blah\" to int"}}
```

Notice that errors from nested data maintain the nested structure.  

`schema/message-map` is so frequently used that shortcuts exist for all the operations.

``` clojure
(schema/coerce-message-map point {:kind :point :x "blah" :y 2})
=> {:x "can't coerce \"blah\" to int"}

(schema/validate-message-map point {:kind :point :x "blah" :y 2})
=> {:x "is invalid"}

(schema/conform-message-map point {:kind :point :x "blah" :y 2})
=> {:x "can't coerce \"blah\" to int"}
```

## Good to Know

We've covered all the major facets of the `schema` library.  There's just a few things you should know before we part ways.    

### Unspecified Fields are Lost

Any extra data you have in your maps, that is not specified in the schema, will be tossed out by the `schema` operations.

``` clojure
(schema/coerce point {:kind :point :x 1 :y 2 :my-extra-data "goes bye bye"})
=> {:kind :point, :x 1, :y 2}
```

### Merging Schema

There are myriad ways to validating or coerce data.  For example, validating form data on the client demands different validations than on the backend where we may need to check for uniqueness in the database. It'd be a shame to duplicate the schemas.  `schema/merge-schemas` allows you to easily patch or modify existing schemas.

``` clojure
(def point {:kind {:type :keyword}
            :x    {:type :int}
            :y    {:type :int}})

(schema/merge-schemas point {:x {:validate even? :message "must be even"}
                             :y {:validate odd? :message "must be odd"}})
=>
{:kind {:type :keyword},
 :x {:type :int,
     :message "must be even",
     :validations [{:validate #object[clojure.core$even_QMARK_ 0x53a9d520 "clojure.core$even_QMARK_@53a9d520"],
                    :message "must be even"}]},
 :y {:type :int,
     :message "must be odd",
     :validations [{:validate #object[clojure.core$odd_QMARK_ 0xe551581 "clojure.core$odd_QMARK_@e551581"],
                    :message "must be odd"}]}}
```

### Self Referencing Schema

Schemas are data.  The shape of the schema data is important.  So `schema` provides a schema to conform your schemas.

``` clojure
(schema/conform-schema! point)
=> {:kind {:type :keyword}, :x {:type :int}, :y {:type :int}}
(schema/conform-schema! {:foo {:type :blah}})
Execution error (ExceptionInfo) at c3kit.apron.schema/conform! (schema.cljc:786).
Unconformable entity
```

One caveat here is that `schema` can only validate one level deep.  This is because the `schema` schema is theoretically infinitely recursive, but in reality it's impossible to make an infinitely nested data structure.

### Shorthands

There are several "shorthands" that abbreviate spec definitions, but they are technically invalid according to the `spec` schema.  `schema/normalize-spec` can be used to expand a single spec and `schema/normalize-schema` will expand all shorthands in the schema.  `schema/conform-schema!` will also normalize shorthands.

```clojure
(schema/normalize-spec {:type [:int] :validate even?})
=>
{:type :seq,
 :spec {:validate #object[clojure.core$even_QMARK_ 0x53a9d520 "clojure.core$even_QMARK_@53a9d520"], :type :int}}

(schema/normalize-spec {:type [{:type :int}] :validate seq})
=>
{:type :seq, :validate #object[clojure.core$seq__5467 0x24f5684c "clojure.core$seq__5467@24f5684c"], :spec {:type :int}}

(schema/normalize-spec {:type {:foo {:type :string}}})
=> {:type :map, :schema {:foo {:type :string}}}

(schema/normalize-spec {:type #{:string :int}})
=> {:type :one-of, :specs [{:type :int} {:type :string}]}
```

Using these shorthands we could define `line` and `polygon` like so:

``` clojure
(def line {:kind  {:type :keyword}
           :start {:type point}
           :end   {:type point}})

(def polygon {:kind   {:type :keyword}
              :points {:type [point]}})
```

This is smaller.  But, as mentioned, it is not valid schema data, and its intent is less clear.  `schema` will accept these shorthands and expand them on the fly. 

### Types

Here is a list of all the possible `:type` values.

``` clojure
:any        ;; no type-coercion or type-validation 
:bigdec     ;; BigDecimal in clj, number in cljs
:boolean    
:date       ;; only use with sql database, java.sql.Date in clj, js/Date in cljs 
:double     ;; number in cljs
:float      ;; number in cljs
:fn         ;; implements iFn
:ignore     ;; same as :any
:instant    ;; java.util.Date in clj, js/Date in cljs
:int        
:keyword        
:kw-ref     ;; same as :keyword, meant to represent a Datomic ident
:long       
:one-of     ;; must also specify :specs
:map        ;; must also specify :schema
:ref        ;; same as :int, meant to store a datomic reference
:seq        ;; must also specify :spec
:string   
:timestamp  ;; only use with sql database, java.sql.Timestamp in clj, js/Date in cljs
:uri        ;; java.net.URI in clj, string in cljs
:uuid       ;; java.util.UUID in clj, cljs.core/UUID in cljs
```

### Integration with Bucket

One of the original uses of `schema` was to move data in and out of databases.  You can see this legacy in several of the `:types` like `:date`, `:timestamp`, `:kw-ref`, and `:ref`.  

[c3kit.bucket](https://github.com/cleancoders/c3kit-bucket) is a library that provides a simple abstract interface that works with a variety of databases including Datomic, Postgresql, MSSQL, H2 or any other JDBC database.  It also includes an in-memory database implementation that makes unit tests screaming fast.  Of course, `bucket` uses `schema` to make sure the data is in the right shape.

![Bucket](https://github.com/cleancoders/c3kit/blob/master/img/bucket_200.png?raw=true)

