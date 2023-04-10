(defsystem :decoder
  :components ((:file "packages")
               (:module "src"
                :serial t
                :components ((:file "decode")))))
