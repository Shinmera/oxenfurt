(asdf:defsystem oxenfurt-core
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Oxenfurt API definitions, client-less."
  :homepage "https://shinmera.com/docs/oxenfurt/"
  :bug-tracker "https://shinmera.com/project/oxenfurt/issues"
  :source-control (:git "https://shinmera.com/project/oxenfurt.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "objects")
               (:file "api")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :alexandria
               :babel
               :yason))

