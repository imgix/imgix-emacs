(require 'imgix)
(require 'ert)
(require 'ht)

(ert-deftest imgix-simple-build-qs-test ()
  (let* ((qs (ht-create)))
    (ht-set! qs "w" "500")
    (ht-set! qs "h" "700")
    (should (string= (imgix-build-qs qs)
                     "h=700&w=500"))))

(ert-deftest imgix-txt-build-qs-test ()
  (let* ((qs (ht-create)))
    (ht-set! qs "txt" "hello there")
    (should (string= (imgix-build-qs qs)
                     "txt=hello%20there"))))

(ert-deftest imgix-parse-qs-test ()
  (let* ((parsed (imgix-parse-qs "txt=hello%20there&w=500&h=250")))

    (should (string= (ht-get parsed "w")
                     "500"))
    (should (string= (ht-get parsed "h")
                     "250"))
    (should (string= (ht-get parsed "txt")
                     "hello%20there"))))

(ert-deftest imgix-is-url-encoded-test ()
  (should (not (imgix-is-url-encoded "hello there")))
  (should (imgix-is-url-encoded "hello%20there")))


(ert-deftest imgix-parse-url-test ()
  (let* ((url "http://jackangers.imgix.net/chester.png?w=500&h=200&fit=crop")
         (parsed (imgix-parse-url url)))
    (should (string= (ht-get parsed "scheme") "http"))
    (should (string= (ht-get parsed "host") "jackangers.imgix.net"))
    (should (string= (ht-get parsed "path") "/chester.png"))
    (should (string= (ht-get parsed "query") "w=500&h=200&fit=crop"))))



(ert-deftest imgix-build-url-test ()
  (let* ((url "http://jackangers.imgix.net/chester.png?w=500&h=200&fit=crop")
         (parsed (imgix-parse-url url)))
    (should (string= (imgix-build-url parsed) url))))
