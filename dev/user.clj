(ns user
  (:require
    [clojure.tools.namespace.repl :as repl]
    [clojure.repl :refer :all]))

(repl/set-refresh-dirs "src" "dev")

(def refresh repl/refresh)
(def refresh-all repl/refresh-all)

