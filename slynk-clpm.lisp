(defpackage :slynk-clpm
  (:use :cl :slynk-api)
  (:export
   #:test))

(in-package :slynk-clpm)

(defslyfun bundle-init (clpmfile asd)
  (clpm-client:bundle-init (pathname clpmfile) :asds asd))

(defslyfun install-context (clpmfile)
  (clpm-client:install :context (pathname clpmfile)))

(defslyfun install-system-from-file (asd)
  (clpm-client:install :asds asd))

(defslyfun active-context ()
  (let ((ctx (clpm-client:active-context)))
    (when ctx
      (namestring ctx))))

(defslyfun activate-context (clpmfile)
  (clpm-client:activate-context (pathname clpmfile)
                                :activate-asdf-integration t))

(defslyfun activate-global-context (context)
  (clpm-client:activate-context context
                                :activate-asdf-integration t))

(defslyfun install-project (project)
  (clpm-client:install :projects project))

(provide 'slynk-clpm)
