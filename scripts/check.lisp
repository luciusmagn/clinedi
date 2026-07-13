;;;; -- Fresh-image test loader --

(require :asdf)
(asdf:load-asd (truename "clinedi.asd"))
(asdf:test-system "clinedi")
