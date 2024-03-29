(asdf:defsystem oxenfurt-dexador
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Dexador client backend for Oxenfurt."
  :homepage "https://Shinmera.github.io/oxenfurt/"
  :bug-tracker "https://github.com/Shinmera/oxenfurt/issues"
  :source-control (:git "https://github.com/Shinmera/oxenfurt.git")
  :serial T
  :components ((:file "dexador"))
  :depends-on (:dexador
               :oxenfurt-core))
