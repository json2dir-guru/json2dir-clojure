# json2dir-clojure

json2dir in Clojure (clojure.core + JDK interop only, hand-written JSON parser): builds the directory tree a JSON document describes.

    java -Dclojure.compile.path=out -cp clojure.jar:spec.alpha.jar:core.specs.alpha.jar:src:out clojure.main -e "(compile 'json2dir.core)"
    java -cp clojure.jar:spec.alpha.jar:core.specs.alpha.jar:out json2dir.core < document.json
