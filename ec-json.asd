(defsystem "ec-json"
  :version "0.1.0"
  :author "Peter Lo"
  :license "MIT License"
  :serial t
  :components ((:module "src"
                :components
                ((:file "gen-json"))))
  :description "A simple JSON encoder for Common Lisp"
  )
