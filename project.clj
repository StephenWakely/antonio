(defproject antonio "0.1.0-SNAPSHOT"
  :description "Clojure wrapper for Figaro"
  :url "http://github.com/fungushumungus/antonio"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.cra.figaro/figaro_2.11 "3.1.0.0"]
                 [t6/from-scala "0.3.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[speclj "3.2.0"]]}}
  :plugins [[speclj "3.2.0"]]
  :test-paths ["spec"])

