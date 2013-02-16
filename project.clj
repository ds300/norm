(defproject norm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.2.1"]
                 [commons-lang/commons-lang "2.6"]
                 [commons-codec/commons-codec "1.6"]
                 [org.clojure/tools.cli "0.2.2"]
                 [uk.ac.susx.mlcl/Byblo "2.1.0"]
                 [cc.mallet/mallet "2.0.7"]
                 [org.clojure/data.xml "0.0.7"]
                 [edu.berkeley.nlp/berkeleylm "1.1.2"]
                 [de.bwaldvogel/liblinear "1.92"]]
  :repositories {"byblo" "http://kungf.eu:8081/nexus/content/groups/public/"}
  :java-source-paths ["src/norm/jvm/"]
  :main norm.core)
