(defproject tachocardia "0.1.0-SNAPSHOT"
  :description "Electronic tachograph card reader and parser"
  :url "https://github.com/jelisavac-l/tachocardia"
  :license {:name "AGPL-3.0-or-later"
            :url "https://www.gnu.org/licenses/agpl-3.0"}
  :dependencies [[org.clojure/clojure "1.12.2"]]
  :main ^:skip-aot tachocardia.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
