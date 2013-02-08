(defproject norm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.2.1"]
                 [commons-lang/commons-lang "2.6"]
                 [commons-codec/commons-codec "1.6"]
                 [org.clojure/tools.cli "0.2.2"]]
  :repositories {"byblo" "http://kungf.eu:8081/nexus/content/groups/public/"}
  :java-source-paths ["src/norm/jvm/"]
  :main norm.core)
