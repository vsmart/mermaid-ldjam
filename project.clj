(defproject mermaid-ldjam "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [quil "2.5.0"]
                 [org.clojure/clojurescript "1.9.293"]]
  :plugins [[lein-cljsbuild "1.1.4"]
            [lein-figwheel "0.5.8"]]
  :hooks [leiningen.cljsbuild]

  :cljsbuild
  {:builds {"dev"
            {:source-paths ["src"]
             :id "dev"
             :figwheel true
             :compiler
             {:output-to "js/main.js"
              :output-dir "out"
              :main "mermaid_ldjam.core"
              :optimizations :none
              :pretty-print true}}
            "min"
            ;; Be aware that you need to change the path in the index.html as well!
            {:source-paths ["src"]
             :id "dev"
             :figwheel true
             :compiler
             {:output-to "js/mermaid.js"
              :output-dir "foo"
              :main "mermaid_ldjam.core"
              :optimizations :advanced
              :pretty-print true}}}})
