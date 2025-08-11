(asdf:defsystem oxenfurt-drakma
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Drakma client backend for Oxenfurt."
  :homepage "https://shinmera.com/docs/oxenfurt/"
  :bug-tracker "https://shinmera.com/project/oxenfurt/issues"
  :source-control (:git "https://shinmera.com/project/oxenfurt.git")
  :serial T
  :components ((:file "drakma"))
  :depends-on (:drakma
               :oxenfurt-core))
