# clj-cronlike

clj-cronlike is a clojure library that allows for the scheduling of
functions to be called using a Vixie Cron-style schedule string.

## Usage

   Add `[cl-cronlike "0.1.7"]` to your project file if you are
   using lien.
```clojure
(ns foo
  (:require [cl-cronlike.core :as cron]))
  
(defn hello! []
  (println "Hello world!" (System/currentTimeMillis)))
  
(let [runner (cron/create-runner)]
  (cron/start-runner runner)
  (cron/add-function runner "0,15,30,45 * * * *" :hello hello!)
  ...
  (cron/remove-function runner :hello)
  (cron/stop-runner runner))
```

   See the docs for more. http://kognate.github.com/clj-cronlike/doc/

## License

Copyright (C) 2012 - 2015 Joshua B. Smith

Distributed under the Eclipse Public License, the same as Clojure.
