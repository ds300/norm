(defproject norm "0.2.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.2.1"]
                 [commons-io/commons-io "2.4"]
                 [commons-lang/commons-lang "2.6"]
                 [commons-codec/commons-codec "1.6"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.dave/cfg "1.0.0"]
                 [uk.ac.susx.mlcl/Byblo "2.1.0"]
                 [cc.mallet/mallet "2.0.7"]
                 [org.clojure/data.xml "0.0.7"]
                 [edu.berkeley.nlp/berkeleylm "1.1.2"]
                 [com.cybozu.labs/langdetect "1.1-20120112"]
                 [de.bwaldvogel/liblinear "1.92"]]
  :repositories {"byblo" "http://kungf.eu:8081/nexus/content/groups/public/"}
  :java-source-paths ["src/norm/jvm/"]
  :javac-options ["-target" "1.6" "-source" "1.6"]
  :profiles {
    :user {:plugins [[lein-midje "3.0.0"]]}
    :dev {
      :dependencies [[midje "1.5.0"]]
    }
  }
  :main norm.core)
