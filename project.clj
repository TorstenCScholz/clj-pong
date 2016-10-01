(defproject clj-pong "0.1.0"
  :description "Very simple pong implementation using Quil as rendering"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [quil "2.4.0"]]
  :aot [clj-pong.core]
  :main clj-pong.core)
